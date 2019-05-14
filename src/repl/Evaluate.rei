type t;

let eval:
  (
	~previous:t=?,
    ~send: Core.Evaluate.result => unit,
    ~complete: Core.Evaluate.evalResult => unit,
    ~readStdout: (module ReadStdout.Sig),
    string
  ) =>
  t;
