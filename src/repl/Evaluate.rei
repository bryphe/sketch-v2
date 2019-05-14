type t;

let empty: t;

/**
 *
 * @param partial - Given a previous state, only apply the evaluation up until the phrase differs from 'previous'
 */
let eval:
  (
    ~partial:bool=?,
    ~previous: t=?,
    ~send: Core.Evaluate.result => unit,
    ~complete: Core.Evaluate.evalResult => unit,
    ~readStdout: (module ReadStdout.Sig),
    string
  ) =>
  t;

let getNextEvalId: unit => Core.Evaluate.evalId;
