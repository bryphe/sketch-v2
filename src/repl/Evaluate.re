open Util;
open Core.Evaluate;

let lastEvalId = ref(0);

let getNextEvalId = () => lastEvalId^ + 1;

type evaluationResult = {
  evalId,
  phrase: Parsetree.toplevel_phrase,
  result: Core.Evaluate.result,
  state: ToploopState.t,
};

type t = list(evaluationResult);

let empty: t = [];

let buffer = Buffer.create(256);
let ppf = Format.formatter_of_buffer(buffer);

/** {2 Communication} */;

/** {2 Communication} */;
let protocolStart = (~blockLoc, ~evalId, ~cached) =>
  Phrase({blockLoc, blockContent: BlockStart, evalId, cached});

let protocolSuccess = (~blockLoc, ~msg, ~warnings, ~stdout, ~evalId) =>
  Phrase({
    blockLoc,
    blockContent: BlockSuccess({msg: msg |> String.trim, warnings, stdout}),
    cached: false,
    evalId,
  });

let protocolError = (~blockLoc, ~error, ~warnings, ~stdout, ~evalId) =>
  Phrase({
    blockLoc,
    blockContent: BlockError({error, warnings, stdout}),
    cached: false,
    evalId,
  });

let updatePhraseCompilationId = (evalId, phrase) =>
  switch (phrase) {
  | Phrase(p) => Phrase({...p, evalId, cached: true})
  | Directive(a, _) => Directive(a, evalId)
  };

/** {2 Execution} */;

/** {2 Execution} */

let warnings = ref([]);

let () =
  Location.warning_printer :=
    (
      (loc, _fmt, w) => {
        switch (Warnings.report(w)) {
        | `Inactive => ()
        | `Active({Warnings.number, message, is_error, sub_locs}) =>
          warnings :=
            [
              {
                warnNumber: number,
                warnMsg: message,
                warnLoc: Core.Loc.toLocation(loc),
                warnSub:
                  sub_locs
                  |> List.map(((loc, msg)) =>
                       (Core.Loc.toLocation(loc), msg)
                     ),
              },
              ...warnings^,
            ]
        };
      }
    );

let rec last = (head, tail) =>
  switch (tail) {
  | [] => head
  | [head, ...tail] => last(head, tail)
  };

let locFromPhrase = {
  fun
  | Parsetree.Ptop_def([]) => None
  | Parsetree.Ptop_def([item, ...items]) => {
      let loc = {
        Location.loc_start: item.pstr_loc.Location.loc_start,
        Location.loc_end: last(item, items).pstr_loc.Location.loc_end,
        Location.loc_ghost: false,
      };
      Some(loc);
    }
  | Ptop_dir(_name, _argument) => None;
};

let eval_phrase = phrase => {
  Warnings.reset_fatal();
  Env.reset_cache_toplevel();
  try (
    {
      let isOk = Toploop.execute_phrase(true, ppf, phrase);
      let message = Buffer.contents(buffer);
      Buffer.clear(buffer);
      Ok((isOk, message));
    }
  ) {
  | exn => Error(exn)
  };
};

let eval =
    (
      ~previous=?,
      ~send: Core.Evaluate.result => unit,
      ~complete: evalResult => unit,
      ~readStdout: (module ReadStdout.Sig),
      code: string,
    )
    : t => {
  warnings := [];

  incr(lastEvalId);
  let evalId = lastEvalId^;

  let previous =
    switch (previous) {
    | None => []
    | Some(v) => v
    };

  module ReadStdout = (val readStdout: ReadStdout.Sig);

  let evaluatePhrase = phrase => {
    let blockLoc =
      locFromPhrase(phrase) |> Option.flatMap(Core.Loc.toLocation);

    send(protocolStart(~blockLoc, ~evalId, ~cached=false));
    /* Redirect stdout */
    let capture = ReadStdout.start();
    let evalResult = eval_phrase(phrase);
    /* Get stdout resut and return stdout back */
    let stdout = ReadStdout.stop(capture);

    let result =
      switch (phrase, evalResult) {
      | (Parsetree.Ptop_dir(_name, _argument), Ok((_, msg))) =>
        /*
         * Directive result could be from anywhere :(
         * #help: stdout
         * #show_val 1: msg
         */
        switch (stdout, msg) {
        | ("", "") => Ok(Directive("unknown", evalId))
        | (msg, "")
        | ("", msg) => Ok(Directive(msg, evalId))
        | (msg1, msg2) => Ok(Directive(msg1 ++ "\n" ++ msg2, evalId))
        }
      | (Parsetree.Ptop_dir(_, _), Error(exn)) =>
        let extractedWarnings = warnings^;
        let {errMsg, _} = Report.reportError(exn);
        Ok(Directive(errMsg, evalId));
      | (Parsetree.Ptop_def(_), Ok((true, msg))) =>
        let extractedWarnings = warnings^;
        Ok(
          protocolSuccess(
            ~blockLoc,
            ~msg,
            ~warnings=extractedWarnings,
            ~stdout,
            ~evalId,
          ),
        );
      | (Parsetree.Ptop_def(_), Ok((false, msg))) =>
        let extractedWarnings = warnings^;
        /* No ideas when this happens */
        Error(
          protocolError(
            ~blockLoc,
            ~error={errMsg: msg, errLoc: None, errSub: []},
            ~warnings=extractedWarnings,
            ~stdout,
            ~evalId,
          ),
        );
      | (Parsetree.Ptop_def(_), Error(exn)) =>
        let extractedWarnings = warnings^;
        let error = Report.reportError(exn);
        Error(
          protocolError(
            ~blockLoc,
            ~error,
            ~warnings=extractedWarnings,
            ~stdout,
            ~evalId,
          ),
        );
      };
    warnings := [];
    result;
  };

  let toString = (v: Parsetree.toplevel_phrase) => {
    switch (v) {
    | Parsetree.Ptop_def(structure) =>
      Pprintast.string_of_structure(structure)
    | Parsetree.Ptop_dir(directive, _) => directive
    };
  };

  let rec loop = (previousPhrases, phrases) => {
    switch (previousPhrases, phrases) {
    | (_, []) =>
      complete(EvalSuccess(evalId));
      [];
    | ([], [phrase, ...tl]) =>
      let result = evaluatePhrase(phrase);
      switch (result) {
      | Ok(v) =>
        let evalResult = {
          phrase,
          result: v,
          state: ToploopState.get(),
          evalId,
        };
        send(v);
        [evalResult, ...loop([], tl)];
      | Error(v) =>
        send(v);
        complete(EvalError(evalId));
        [];
      };
    | ([previousPhrase, ...previousTail], [phrase, ...tl]) =>
      let s1 = toString(previousPhrase.phrase);
      let s2 = toString(phrase);
      /* TODO: Is there a better way to compare these? */
      if (String.equal(s1, s2)) {
        let blockLoc =
          locFromPhrase(previousPhrase.phrase)
          |> Option.flatMap(Core.Loc.toLocation);

        send(protocolStart(~blockLoc, ~cached=true, ~evalId));
        send(previousPhrase.result |> updatePhraseCompilationId(evalId));
        ToploopState.set(previousPhrase.state);
        [previousPhrase, ...loop(previousTail, tl)];
      } else {
        let result = evaluatePhrase(phrase);
        switch (result) {
        | Ok(v) =>
          send(v);
          let evalResult = {
            phrase,
            result: v,
            state: ToploopState.get(),
            evalId,
          };
          send(v);
          [evalResult, ...loop([], tl)];
        | Error(v) =>
          send(v);
          complete(EvalError(evalId));
          [];
        };
      };
    };
  };

  let result =
    try (
      {
        let filename = "//toplevel//";
        let lexbuf = Lexing.from_string(code);
        Location.init(lexbuf, filename);
        Location.input_name := filename;
        Location.input_lexbuf := Some(lexbuf);
        loop(previous, Toploop.parse_use_file^(lexbuf));
      }
    ) {
    | Sys.Break =>
      complete(EvalInterupted(evalId));
      previous;
    | exn =>
      let extractedWarnings = warnings^;
      let error = Report.reportError(exn);
      send(
        protocolError(
          ~blockLoc=None,
          ~error,
          ~warnings=extractedWarnings,
          ~stdout="",
          ~evalId,
        ),
      );
      warnings := [];
      complete(EvalError(evalId));
      previous;
    };
  result;
};
