structure Sql = struct

open Mono

val debug = ref false

type lvar = int

datatype func =
         DtCon0 of string
       | DtCon1 of string
       | UnCon of string
       | Other of string

datatype exp =
         Const of Prim.t
       | Var of int
       | Lvar of lvar
       | Func of func * exp list
       | Recd of (string * exp) list
       | Proj of exp * string

datatype reln =
         Known
       | Sql of string
       | PCon0 of string
       | PCon1 of string
       | Eq
       | Ne
       | Lt
       | Le
       | Gt
       | Ge

datatype prop =
         True
       | False
       | Unknown
       | And of prop * prop
       | Or of prop * prop
       | Reln of reln * exp list
       | Cond of exp * prop

datatype chunk =
         String of string
       | Exp of Mono.exp

fun chunkify e =
    case #1 e of
        EPrim (Prim.String s) => [String s]
      | EStrcat (e1, e2) =>
        let
            val chs1 = chunkify e1
            val chs2 = chunkify e2
        in
            case chs2 of
                String s2 :: chs2' =>
                (case List.last chs1 of
                     String s1 => List.take (chs1, length chs1 - 1) @ String (s1 ^ s2) :: chs2'
                   | _ => chs1 @ chs2)
              | _ => chs1 @ chs2
        end
      | _ => [Exp e]

type 'a parser = chunk list -> ('a * chunk list) option

fun always v chs = SOME (v, chs)

fun parse p s =
    case p (chunkify s) of
        SOME (v, []) => SOME v
      | _ => NONE

fun const s chs =
    case chs of
        String s' :: chs => if String.isPrefix s s' then
                                SOME ((), if size s = size s' then
                                              chs
                                          else
                                              String (String.extract (s', size s, NONE)) :: chs)
                            else
                                NONE
      | _ => NONE

fun follow p1 p2 chs =
    case p1 chs of
        NONE => NONE
      | SOME (v1, chs) =>
        case p2 chs of
            NONE => NONE
          | SOME (v2, chs) => SOME ((v1, v2), chs)

fun wrap p f chs =
    case p chs of
        NONE => NONE
      | SOME (v, chs) => SOME (f v, chs)

fun wrapP p f chs =
    case p chs of
        NONE => NONE
      | SOME (v, chs) =>
        case f v of
            NONE => NONE
          | SOME r => SOME (r, chs)

fun alt p1 p2 chs =
    case p1 chs of
        NONE => p2 chs
      | v => v

fun altL ps =
    case rev ps of
        [] => (fn _ => NONE)
      | p :: ps =>
        foldl (fn (p1, p2) => alt p1 p2) p ps

fun opt p chs =
    case p chs of
        NONE => SOME (NONE, chs)
      | SOME (v, chs) => SOME (SOME v, chs)

fun skip cp chs =
    case chs of
        String "" :: chs => skip cp chs
      | String s :: chs' => if cp (String.sub (s, 0)) then
                                skip cp (String (String.extract (s, 1, NONE)) :: chs')
                            else
                                SOME ((), chs)
      | _ => SOME ((), chs)

fun keep cp chs =
    case chs of
        String "" :: chs => keep cp chs
      | String s :: chs' =>
        let
            val (befor, after) = Substring.splitl cp (Substring.full s)
        in
            if Substring.isEmpty befor then
                NONE
            else
                SOME (Substring.string befor,
                      if Substring.isEmpty after then
                          chs'
                      else
                          String (Substring.string after) :: chs')
        end
      | _ => NONE

fun ws p = wrap (follow (skip (fn ch => ch = #" "))
                        (follow p (skip (fn ch => ch = #" ")))) (#1 o #2)

fun log name p chs =
    (if !debug then
         (print (name ^ ": ");
          app (fn String s => print s
                | _ => print "???") chs;
          print "\n")
     else
         ();
     p chs)

fun list p chs =
    altL [wrap (follow p (follow (ws (const ",")) (list p)))
               (fn (v, ((), ls)) => v :: ls),
          wrap (ws p) (fn v => [v]),
          always []] chs

val ident = keep (fn ch => Char.isAlphaNum ch orelse ch = #"_")

val t_ident = wrapP ident (fn s => if String.isPrefix "T_" s then
                                       SOME (String.extract (s, 2, NONE))
                                   else
                                       NONE)
val uw_ident = wrapP ident (fn s => if String.isPrefix "uw_" s andalso size s >= 4 then
                                        SOME (str (Char.toUpper (String.sub (s, 3)))
                                              ^ String.extract (s, 4, NONE))
                                    else
                                        NONE)

val field = wrap (follow t_ident
                         (follow (const ".")
                                 uw_ident))
                 (fn (t, ((), f)) => (t, f))

datatype Rel =
         Exps of exp * exp -> prop
       | Props of prop * prop -> prop

datatype sqexp =
         SqConst of Prim.t
       | SqTrue
       | SqFalse
       | SqNot of sqexp
       | Field of string * string
       | Computed of string
       | Binop of Rel * sqexp * sqexp
       | SqKnown of sqexp
       | Inj of Mono.exp
       | SqFunc of string * sqexp
       | Unmodeled
       | Null

fun cmp s r = wrap (const s) (fn () => Exps (fn (e1, e2) => Reln (r, [e1, e2])))

val sqbrel = altL [cmp "=" Eq,
                   cmp "<>" Ne,
                   cmp "<=" Le,
                   cmp "<" Lt,
                   cmp ">=" Ge,
                   cmp ">" Gt,
                   wrap (const "AND") (fn () => Props And),
                   wrap (const "OR") (fn () => Props Or)]

datatype ('a, 'b) sum = inl of 'a | inr of 'b

fun string chs =
    case chs of
        String s :: chs =>
        if size s >= 2 andalso String.sub (s, 0) = #"'" then
            let
                fun loop (cs, acc) =
                    case cs of
                        [] => NONE
                      | c :: cs =>
                        if c = #"'" then
                            SOME (String.implode (rev acc), cs)
                        else if c = #"\\" then
                            case cs of
                                c :: cs => loop (cs, c :: acc)
                              | _ => raise Fail "Iflow.string: Unmatched backslash escape"
                        else
                            loop (cs, c :: acc)
            in
                case loop (String.explode (String.extract (s, 1, NONE)), []) of
                    NONE => NONE
                  | SOME (s, []) => SOME (s, chs)
                  | SOME (s, cs) => SOME (s, String (String.implode cs) :: chs)
            end
        else
            NONE
      | _ => NONE                            

val prim =
    altL [wrap (follow (wrapP (follow (keep Char.isDigit) (follow (const ".") (keep Char.isDigit)))
                              (fn (x, ((), y)) => Option.map Prim.Float (Real64.fromString (x ^ "." ^ y))))
                       (opt (const "::float8"))) #1,
          wrap (follow (wrapP (keep Char.isDigit)
                              (Option.map Prim.Int o Int64.fromString))
                       (opt (const "::int8"))) #1,
          wrap (follow (opt (const "E")) (follow string (opt (const "::text"))))
               (Prim.String o #1 o #2)]

fun known' chs =
    case chs of
        Exp (EFfi ("Basis", "sql_known"), _) :: chs => SOME ((), chs)
      | _ => NONE

fun sqlify chs =
    case chs of
        Exp (EFfiApp ("Basis", f, [(e, _)]), _) :: chs =>
        if String.isPrefix "sqlify" f then
            SOME (e, chs)
        else
            NONE
      | Exp (ECase (e, [((PCon (_, PConFfi {mod = "Basis", con = "True", ...}, NONE), _),
                         (EPrim (Prim.String "TRUE"), _)),
                        ((PCon (_, PConFfi {mod = "Basis", con = "False", ...}, NONE), _),
                         (EPrim (Prim.String "FALSE"), _))], _), _) :: chs =>
        SOME (e, chs)
                          
      | _ => NONE

fun constK s = wrap (const s) (fn () => s)

val funcName = altL [constK "COUNT",
                     constK "MIN",
                     constK "MAX",
                     constK "SUM",
                     constK "AVG"]

val unmodeled = altL [const "COUNT(*)",
                      const "CURRENT_TIMESTAMP"]

fun sqexp chs =
    log "sqexp"
    (altL [wrap prim SqConst,
           wrap (const "TRUE") (fn () => SqTrue),
           wrap (const "FALSE") (fn () => SqFalse),
           wrap (const "NULL") (fn () => Null),
           wrap field Field,
           wrap uw_ident Computed,
           wrap known SqKnown,
           wrap func SqFunc,
           wrap unmodeled (fn () => Unmodeled),
           wrap sqlify Inj,
           wrap (follow (const "COALESCE(") (follow sqexp (follow (const ",")
                                                                  (follow (keep (fn ch => ch <> #")")) (const ")")))))
                (fn ((), (e, _)) => e),
           wrap (follow (const "(NOT ") (follow sqexp (const ")")))
                (fn ((), (e, _)) => SqNot e),
           wrap (follow (ws (const "("))
                        (follow (wrap
                                     (follow sqexp
                                             (alt
                                                  (wrap
                                                       (follow (ws sqbrel)
                                                               (ws sqexp))
                                                       inl)
                                                  (always (inr ()))))
                                     (fn (e1, sm) =>
                                         case sm of
                                             inl (bo, e2) => Binop (bo, e1, e2)
                                           | inr () => e1))
                                (const ")")))
                (fn ((), (e, ())) => e)])
    chs

and known chs = wrap (follow known' (follow (const "(") (follow sqexp (const ")"))))
                     (fn ((), ((), (e, ()))) => e) chs
                
and func chs = wrap (follow funcName (follow (const "(") (follow sqexp (const ")"))))
                    (fn (f, ((), (e, ()))) => (f, e)) chs

datatype sitem =
         SqField of string * string
       | SqExp of sqexp * string

val sitem = alt (wrap (follow sqexp (follow (const " AS ") uw_ident))
                      (fn (e, ((), s)) => SqExp (e, s)))
                (wrap field SqField)

val select = log "select"
             (wrap (follow (const "SELECT ") (list sitem))
                   (fn ((), ls) => ls))

val fitem = wrap (follow uw_ident
                         (follow (const " AS ")
                                 t_ident))
                 (fn (t, ((), f)) => (t, f))

val from = log "from"
           (wrap (follow (const "FROM ") (list fitem))
                 (fn ((), ls) => ls))

val wher = wrap (follow (ws (const "WHERE ")) sqexp)
           (fn ((), ls) => ls)

type query1 = {Select : sitem list,
              From : (string * string) list,
              Where : sqexp option}

val query1 = log "query1"
                (wrap (follow (follow select from) (opt wher))
                      (fn ((fs, ts), wher) => {Select = fs, From = ts, Where = wher}))

datatype query =
         Query1 of query1
       | Union of query * query

val orderby = log "orderby"
              (wrap (follow (ws (const "ORDER BY "))
                            (follow (list sqexp)
                                    (opt (ws (const "DESC")))))
                    ignore)

fun query chs = log "query"
                (wrap
                     (follow
                          (alt (wrap (follow (const "((")
                                             (follow query
                                                     (follow (const ") UNION (")
                                                             (follow query (const "))")))))
                                     (fn ((), (q1, ((), (q2, ())))) => Union (q1, q2)))
                               (wrap query1 Query1))
                          (opt orderby))
                     #1)
                chs

datatype dml =
         Insert of string * (string * sqexp) list
       | Delete of string * sqexp
       | Update of string * (string * sqexp) list * sqexp

val insert = log "insert"
             (wrapP (follow (const "INSERT INTO ")
                            (follow uw_ident
                                    (follow (const " (")
                                            (follow (list uw_ident)
                                                    (follow (const ") VALUES (")
                                                            (follow (list sqexp)
                                                                    (const ")")))))))
              (fn ((), (tab, ((), (fs, ((), (es, ())))))) =>
                  (SOME (tab, ListPair.zipEq (fs, es)))
                  handle ListPair.UnequalLengths => NONE))

val delete = log "delete"
                 (wrap (follow (const "DELETE FROM ")
                               (follow uw_ident
                                       (follow (const " AS T_T WHERE ")
                                               sqexp)))
                       (fn ((), (tab, ((), es))) => (tab, es)))

val setting = log "setting"
              (wrap (follow uw_ident (follow (const " = ") sqexp))
               (fn (f, ((), e)) => (f, e)))

val update = log "update"
                 (wrap (follow (const "UPDATE ")
                               (follow uw_ident
                                       (follow (const " AS T_T SET ")
                                               (follow (list setting)
                                                       (follow (ws (const "WHERE "))
                                                               sqexp)))))
                       (fn ((), (tab, ((), (fs, ((), e))))) =>
                           (tab, fs, e)))

val dml = log "dml"
              (altL [wrap insert Insert,
                     wrap delete Delete,
                     wrap update Update])

datatype querydml =
         Query of query
       | Dml of dml

val querydml = log "querydml" (altL [wrap dml Dml, wrap query Query])

end
