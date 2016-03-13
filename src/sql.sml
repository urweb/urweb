structure Sql :> SQL = struct

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

datatype cmp =
         Eq
       | Ne
       | Lt
       | Le
       | Gt
       | Ge

datatype reln =
         Known
       | Sql of string
       | PCon0 of string
       | PCon1 of string
       | Cmp of cmp

datatype lop =
         And
       | Or

datatype prop =
         True
       | False
       | Unknown
       | Lop of lop * prop * prop
       | Reln of reln * exp list
       | Cond of exp * prop

datatype chunk =
         String of string
       | Exp of Mono.exp

fun chunkify e =
    case #1 e of
        EPrim (Prim.String (_, s)) => [String s]
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

(* Used by primSqlcache. *)
fun optConst s chs =
    case chs of
        String s' :: chs => if String.isPrefix s s' then
                                SOME (s, if size s = size s' then
                                              chs
                                          else
                                              String (String.extract (s', size s, NONE)) :: chs)
                            else
                                SOME ("", String s' :: chs)
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
                                        SOME s)

val field = wrap (follow (opt (follow t_ident (const ".")))
                         uw_ident)
                 (fn (SOME (t, ()), f) => (t, f)
                   | (NONE, f) => ("T", f)) (* Should probably deal with this MySQL/SQLite case better some day. *)

datatype Rel =
         RCmp of cmp
       | RLop of lop

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

fun cmp s r = wrap (const s) (fn () => RCmp r)

val sqbrel = altL [cmp "=" Eq,
                   cmp "IS NOT DISTINCT FROM" Eq,
                   cmp "<>" Ne,
                   cmp "<=" Le,
                   cmp "<" Lt,
                   cmp ">=" Ge,
                   cmp ">" Gt,
                   wrap (const "AND") (fn () => RLop And),
                   wrap (const "OR") (fn () => RLop Or)]

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
               ((fn s => Prim.String (Prim.Normal, s)) o #1 o #2)]

val primSqlcache =
    (* Like [prim], but always uses [Prim.String]s. *)
    let
        fun wrapS p f = wrap p ((fn s => Prim.String (Prim.Normal, s)) o f)
    in
        altL [wrapS (follow (wrap (follow (keep Char.isDigit)
                                          (follow (const ".") (keep Char.isDigit)))
                                  (fn (x, ((), y)) => x ^ "." ^ y))
                            (optConst "::float8"))
                    op^,
              wrapS (follow (keep Char.isDigit)
                            (optConst "::int8"))
                    op^,
              wrapS (follow (optConst "E") (follow string (optConst "::text")))
                    (fn (c1, (s, c2)) => c1 ^ s ^ c2)]
end

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
                         (EPrim (Prim.String (Prim.Normal, "TRUE")), _)),
                        ((PCon (_, PConFfi {mod = "Basis", con = "False", ...}, NONE), _),
                         (EPrim (Prim.String (Prim.Normal, "FALSE")), _))], _), _) :: chs =>
        SOME (e, chs)

      | _ => NONE

(* For sqlcache, we only care that we can do string equality on injected Mono
   expressions, so accept any expression without modifying it. *)
val sqlifySqlcache =
 fn Exp e :: chs => SOME (e, chs)
  | _ => NONE

fun constK s = wrap (const s) (fn () => s)

val funcName = altL [constK "COUNT",
                     constK "MIN",
                     constK "MAX",
                     constK "SUM",
                     constK "AVG"]

fun arithmetic pExp = follow (const "(")
                             (follow pExp
                                     (follow (altL (map const [" + ", " - ", " * ", " / ", " >> ", " << "]))
                                             (follow pExp (const ")"))))

val unmodeled = altL [const "COUNT(*)",
                      const "CURRENT_TIMESTAMP"]

val sqlcacheMode = ref false;

fun sqexp chs =
    log "sqexp"
    (altL [wrap (if !sqlcacheMode then primSqlcache else prim) SqConst,
           wrap (const "TRUE") (fn () => SqTrue),
           wrap (const "FALSE") (fn () => SqFalse),
           wrap (follow (const "NULL::") ident) (fn ((), _) => Null),
           wrap (const "NULL") (fn () => Null),
           wrap known SqKnown,
           wrap func SqFunc,
           wrap field Field,
           wrap uw_ident Computed,
           wrap (arithmetic sqexp) (fn _ => Unmodeled),
           wrap unmodeled (fn () => Unmodeled),
           wrap (if !sqlcacheMode then sqlifySqlcache else sqlify) Inj,
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

datatype jtype = Inner | Left | Right | Full

datatype fitem =
         Table of string * string (* table AS name *)
       | Join of jtype * fitem * fitem * sqexp
       | Nested of query * string (* query AS name *)

     and query =
         Query1 of {Select : sitem list, From : fitem list, Where : sqexp option}
       | Union of query * query

val wher = wrap (follow (ws (const "WHERE ")) sqexp)
           (fn ((), ls) => ls)

val orderby = log "orderby"
              (wrap (follow (ws (const "ORDER BY "))
                            (list (follow sqexp
                                          (opt (ws (const "DESC"))))))
                    ignore)

val groupby = log "groupby"
                  (wrap (follow (ws (const "GROUP BY "))
                                (list sqexp))
                        ignore)

val jtype = altL [wrap (const "JOIN") (fn () => Inner),
                  wrap (const "LEFT JOIN") (fn () => Left),
                  wrap (const "RIGHT JOIN") (fn () => Right),
                  wrap (const "FULL JOIN") (fn () => Full)]

fun fitem chs = altL [wrap (follow uw_ident
                                   (follow (const " AS ")
                                           t_ident))
                           (fn (t, ((), f)) => Table (t, f)),
                      wrap (follow (const "(")
                                   (follow fitem
                                           (follow (ws jtype)
                                                   (follow fitem
                                                           (follow (const " ON ")
                                                                   (follow sqexp
                                                                           (const ")")))))))
                           (fn ((), (fi1, (jt, (fi2, ((), (se, ())))))) =>
                               Join (jt, fi1, fi2, se)),
                      wrap (follow (const "(")
                                   (follow query
                                           (follow (const ") AS ") t_ident)))
                           (fn ((), (q, ((), f))) => Nested (q, f))]
                     chs

and query1 chs = log "query1"
                     (wrap (follow (follow select from) (opt wher))
                           (fn ((fs, ts), wher) => {Select = fs, From = ts, Where = wher}))
                     chs

and from chs = log "from"
                   (wrap (follow (const "FROM ") (list fitem))
                         (fn ((), ls) => ls))
                   chs

and query chs = log "query"
                    (wrap (follow
                               (alt (wrap (follow (const "((")
                                                  (follow query
                                                          (follow (const ") UNION (")
                                                                  (follow query (const "))")))))
                                          (fn ((), (q1, ((), (q2, ())))) => Union (q1, q2)))
                                    (wrap query1 Query1))
                               (follow (opt groupby) (opt orderby)))
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
                                       (follow (opt (const " AS T_T"))
                                               (opt (follow (const " WHERE ") sqexp)))))
                       (fn ((), (tab, (_, wher))) => (tab, case wher of
                                                               SOME (_, es) => es
                                                             | NONE => SqTrue)))

val setting = log "setting"
                  (wrap (follow uw_ident (follow (const " = ") sqexp))
                        (fn (f, ((), e)) => (f, e)))

val update = log "update"
                 (wrap (follow (const "UPDATE ")
                               (follow uw_ident
                                       (follow (follow (opt (const " AS T_T")) (const " SET "))
                                               (follow (list setting)
                                                       (follow (ws (const "WHERE "))
                                                               sqexp)))))
                       (fn ((), (tab, (_, (fs, ((), e))))) =>
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
