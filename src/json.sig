signature JSON = sig 
  datatype json =
          Array of json list
          | Null
          | Float of real
          | String of string
          | Bool of bool
          | Int of int
          | Obj of (string * json) list

  val parse: string -> json
  val print: json -> string
end
