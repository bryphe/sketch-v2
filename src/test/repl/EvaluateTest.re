open TestFramework;
open Core.Evaluate;

let initialize = () => {
  Toploop.initialize_toplevel_env();
  Toploop.input_name := "//toplevel//";
  Repl.SyntaxControl.re();
};

let eval = Repl.Evaluate.eval(~readStdout=(module ReadStdoutUnix));
let getNextEvalId = Repl.Evaluate.getNextEvalId;

let makeLoc = (locStart, locEnd) => {
  Core.Loc.{
    locStart: {
      line: fst(locStart),
      col: snd(locStart),
    },
    locEnd: {
      line: fst(locEnd),
      col: snd(locEnd),
    },
  };
};

let makeWarning = (~sub=[], ~number, ~msg, block_start, block_end) => {
  {
    warnLoc: Some(makeLoc(block_start, block_end)),
    warnNumber: number,
    warnMsg: msg,
    warnSub: sub,
  };
};

let success = (~evalId=0, ~cached=false, ~warnings=[], ~stdout="", msg, block_start, block_end) =>
  Phrase({
	evalId,
	cached,
    blockLoc: Some(makeLoc(block_start, block_end)),
    blockContent: BlockSuccess({msg, warnings, stdout}),
  });

let error =
    (
	  ~evalId=0,
	  ~cached=false,
      ~warnings=[],
      ~stdout="",
      ~errSub=[],
      msg,
      block_loc,
      error_start,
      error_end,
    ) => {
  Phrase({
    blockLoc:
      block_loc
      |> Util.Option.map(((block_start, block_end)) =>
           makeLoc(block_start, block_end)
         ),
	evalId,
	cached,
    blockContent:
      BlockError({
        error: {
          errLoc: Some(makeLoc(error_start, error_end)),
          errMsg: msg,
          errSub,
        },
        warnings,
        stdout,
      }),
  });
};
describe("incremental test test", ({test, _}) =>
  test("single line, multiple phrases", ({expect}) => {
    initialize();

    let mock0 = Mock.mock1(_ => ());
    let mockComplete0 = Mock.mock1(_ => ());

    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

    let previous =
      eval(
        ~send=Mock.fn(mock0),
        ~complete=Mock.fn(mockComplete0),
        "let x = 1; let y = x + 1;",
      );
	let evalId = getNextEvalId();
    let _ =
      eval(
        ~previous,
        ~send=Mock.fn(mock),
        ~complete=Mock.fn(mockComplete),
        "let x = 1; let y = x + 1; let z = y + 1;",
      );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledTimes(6);
    let calls = Mock.getCalls(mock) |> List.rev;

    expect.equal(
      List.nth(calls, 1),
      success(~evalId, ~cached=true, "let x: int = 1;", (0, 0), (0, 8)),
    );
    expect.equal(
      List.nth(calls, 3),
      success(~evalId, ~cached=true, "let y: int = 2;", (0, 11), (0, 23)),
    );
    expect.equal(
      List.nth(calls, 5),
      success(~evalId, ~cached=false, "let z: int = 3;", (0, 26), (0, 38)),
    );
  })
);

describe("success test", ({test, _}) => {
  test("single line, multiple phrases", ({expect}) => {
    initialize();

    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();
    eval(
      ~send=Mock.fn(mock),
      ~complete=Mock.fn(mockComplete),
      "let x = 1; let y = 2; let z = 3;",
    );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledTimes(6);
    let calls = Mock.getCalls(mock) |> List.rev;

    expect.equal(
      List.nth(calls, 1),
      success(~evalId, "let x: int = 1;", (0, 0), (0, 8)),
    );
    expect.equal(
      List.nth(calls, 3),
      success(~evalId, "let y: int = 2;", (0, 11), (0, 19)),
    );
    expect.equal(
      List.nth(calls, 5),
      success(~evalId, "let z: int = 3;", (0, 22), (0, 30)),
    );
  });

  test("multiple lines, multiple phrases", ({expect}) => {
    initialize();

    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();
    eval(
      ~send=Mock.fn(mock),
      ~complete=Mock.fn(mockComplete),
      "let x = 1;\nlet y = 2;\nlet z = 3;",
    );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledTimes(6);
    let calls = Mock.getCalls(mock) |> List.rev;

    expect.equal(
      List.nth(calls, 1),
      success(~evalId, "let x: int = 1;", (0, 0), (0, 8)),
    );
    expect.equal(
      List.nth(calls, 3),
      success(~evalId, "let y: int = 2;", (1, 0), (1, 8)),
    );
    expect.equal(
      List.nth(calls, 5),
      success(~evalId, "let z: int = 3;", (2, 0), (2, 8)),
    );
  });

  test("single phrases in multiple lines", ({expect}) => {
    initialize();
    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();
    eval(
      ~send=Mock.fn(mock),
      ~complete=Mock.fn(mockComplete),
      "let myFunc = () => {\n  1\n}",
    );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledTimes(2);
    let calls = Mock.getCalls(mock) |> List.rev;

    expect.equal(
      List.nth(calls, 1),
      success(~evalId, "let myFunc: unit => int = <fun>;", (0, 0), (2, 0)),
    );
  });

  test("with warnings", ({expect}) => {
    initialize();
    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();
    eval(
      ~send=Mock.fn(mock),
      ~complete=Mock.fn(mockComplete),
      "type warn = | Foo | Bar;\nfun | Foo => () | Bar => () | _ => ();",
    );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledTimes(4);
    let calls = Mock.getCalls(mock) |> List.rev;

    expect.equal(
      List.nth(calls, 3),
      success(
		~evalId,
        ~warnings=[
          makeWarning(
            ~number=11,
            ~msg="this match case is unused.",
            (1, 28),
            (1, 30),
          ),
        ],
        "- : warn => unit = <fun>",
        (1, 0),
        (1, 36),
      ),
    );
  });
});

describe("error tests", ({test, _}) => {
  test("syntax error", ({expect}) => {
    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();
    eval(~send=Mock.fn(mock), ~complete=Mock.fn(mockComplete), "let a = {");
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalError(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledTimes(1);
    expect.mock(mock).toBeCalledWith(
      error(
		~evalId,
        ~errSub=[
          (Some(makeLoc((0, 8), (0, 8))), "This '{' might be unmatched"),
        ],
        "Syntax error: '}' expected",
        None,
        (0, 8),
        (0, 8),
      ),
    );
  });

  test("single line, error as last phrase", ({expect}) => {
    initialize();

    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();

    eval(
      ~send=Mock.fn(mock),
      ~complete=Mock.fn(mockComplete),
      "let a = 1; let b = \"2\"; a + b;",
    );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalError(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledTimes(6);

    let calls = Mock.getCalls(mock) |> List.rev;

    expect.equal(
      List.nth(calls, 1),
      success(~evalId, "let a: int = 1;", (0, 0), (0, 8)),
    );

    expect.equal(
      List.nth(calls, 3),
      success(~evalId, "let b: string = \"2\";", (0, 11), (0, 21)),
    );

    expect.equal(
      List.nth(calls, 5),
      error(
		~evalId,
        "This expression has type string but an expression was expected of type\n         int",
        Some(((0, 24), (0, 28))),
        (0, 28),
        (0, 28),
      ),
    );
  });
});

describe("stdout", ({test, _}) =>
  test("redirect stdout", ({expect}) => {
    initialize();
    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();
    eval(
      ~send=Mock.fn(mock),
      ~complete=Mock.fn(mockComplete),
      "print_endline(\"Hello world\")",
    );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledTimes(2);

    let calls = Mock.getCalls(mock) |> List.rev;
    expect.equal(
      List.nth(calls, 1),
      success(~evalId, ~stdout="Hello world\n", "- : unit = ()", (0, 0), (0, 27)),
    );
  })
);

describe("directives", ({test, _}) => {
  test("directive ouutput to stdout", ({expect}) => {
    initialize();
    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();
    eval(
      ~send=Mock.fn(mock),
      ~complete=Mock.fn(mockComplete),
      "let a = 1;\n#show_val a;",
    );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledTimes(4);
    expect.mock(mock).toBeCalledWith(Directive("let a: int;\n", evalId));
  });

  test("directive output to Toploop.execute_phrase buffer", ({expect}) => {
    initialize();
    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();
    eval(
      ~send=Mock.fn(mock),
      ~complete=Mock.fn(mockComplete),
      "#show_val 1;",
    );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledWith(
      Directive("Wrong type of argument for directive `show_val'.\n", evalId),
    );
  });

  test("directive with error", ({expect}) => {
    initialize();
    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();
    eval(
      ~send=Mock.fn(mock),
      ~complete=Mock.fn(mockComplete),
      "#show_val a;",
    );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess(evalId));
    /* Inspect each block calls */

    expect.mock(mock).toBeCalledWith(Directive("Unbound value a", evalId));
  });

  test(
    "ignore directives with error and continue the evaluation", ({expect}) => {
    initialize();
    let mock = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

	let evalId = getNextEvalId();
    eval(
      ~send=Mock.fn(mock),
      ~complete=Mock.fn(mockComplete),
      "#show_val a; let x = 1; let y = 2;",
    );
    /* Inspect overal result */
    expect.mock(mockComplete).toBeCalledTimes(1);
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess(evalId));
    /* Inspect each block calls */
    expect.mock(mock).toBeCalledTimes(6);
    expect.mock(mock).toBeCalledWith(Directive("Unbound value a", evalId));
    let calls = Mock.getCalls(mock) |> List.rev;
    expect.equal(
      List.nth(calls, 3),
      success(~evalId, "let x: int = 1;", (0, 13), (0, 21)),
    );
  });
});
