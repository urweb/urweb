signature FROMJSON = sig 
    val getO: string -> Json.json -> Json.json option
    val get: string -> Json.json -> Json.json
    val asInt: Json.json -> int
    val asString: Json.json -> string
    val asOptionalInt: Json.json -> int option
    val asOptionalString: Json.json -> string option
end
