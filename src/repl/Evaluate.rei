type t;

let eval:
  (
	~previous:option(t)=?,
    ~send: Core.Evaluate.result => unit,
    ~complete: Core.Evaluate.evalResult => unit,
    ~readStdout: (module ReadStdout.Sig),
    string
  ) =>
  t;
