[@deriving show]
type sub = list((option(Loc.t), string));

[@deriving show]
type error = {
  errLoc: option(Loc.t),
  errMsg: string,
  errSub: sub,
};

[@deriving show]
type warning = {
  warnLoc: option(Loc.t),
  warnNumber: int,
  warnMsg: string,
  warnSub: sub,
};

[@deriving show]
type evalId = int;

[@deriving show]
type blockContent =
  | BlockStart
  | BlockSuccess{
      msg: string,
      warnings: list(warning),
      stdout: string,
    }
  | BlockError{
      error,
      warnings: list(warning),
      stdout: string,
    };

[@deriving show]
type result =
  | Phrase{
      blockLoc: option(Loc.t),
      blockContent,
      evalId,
      cached: bool,
    }
  | Directive(string, evalId);

type evalResult =
  | EvalSuccess(evalId)
  | EvalError(evalId)
  | EvalInterupted(evalId);
