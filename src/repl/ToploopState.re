type t = {
	env: Env.t,
	value: Obj.t,
};

let get = () => {
	{
		env: Toploop.toplevel_env^,
		value: Obj.magic(Toploop.toplevel_value_bindings^),
	};
};

let set = ({env, value}) => {
	Toploop.toplevel_env := env;
	Toploop.toplevel_value_bindings := Obj.magic(value);
};
