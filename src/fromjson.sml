structure FromJson :> FROMJSON = struct 
fun getO (s: string) (l: Json.json): Json.json option = 
    case l of
        Json.Obj pairs =>
        (case List.find (fn tup => #1 tup = s) pairs of
             NONE => NONE
           | SOME tup => SOME (#2 tup))
      | _ => raise Fail ("Expected JSON object, got: " ^ Json.print l)
fun get (s: string) (l: Json.json): Json.json =
        (case getO s l of
             NONE => raise Fail ("Failed to find JSON object key " ^ s ^ " in " ^ Json.print l)
           | SOME a => a)

fun asInt (j: Json.json): int = 
    case j of
        Json.Int i => i
     |  _ => raise Fail ("Expected JSON int, got: " ^ Json.print j)

fun asString (j: Json.json): string =
    case j of
        Json.String s => s
     |  _ => raise Fail ("Expected JSON string, got: " ^ Json.print j)

fun asOptionalInt (j: Json.json): int option =
    case j of
        Json.Null => NONE
     |  Json.Int i => SOME i
     |  _ => raise Fail ("Expected JSON int or null, got: " ^ Json.print j)

fun asOptionalString (j: Json.json): string option =
    case j of
        Json.Null => NONE
     |  Json.String s => SOME s
     |  _ => raise Fail ("Expected JSON string or null, got: " ^ Json.print j)
end
