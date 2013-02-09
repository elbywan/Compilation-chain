let value_kind = Identifier.fresh_kind "value"

let function_kind = Identifier.fresh_kind "function"

let fresh () = Identifier.fresh value_kind

let fresh_function () = Identifier.fresh ~prefix:"`f" function_kind
