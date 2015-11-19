signature SQL = sig

val debug : bool ref

val sqlcacheMode : bool ref

datatype chunk =
         String of string
       | Exp of Mono.exp

val chunkify : Mono.exp -> chunk list

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

type 'a parser

val parse : 'a parser -> Mono.exp -> 'a option

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

datatype ('a,'b) sum = inl of 'a | inr of 'b

datatype sitem =
         SqField of string * string
       | SqExp of sqexp * string

datatype jtype = Inner | Left | Right | Full

datatype fitem =
         Table of string * string (* table AS name *)
       | Join of jtype * fitem * fitem * sqexp
       | Nested of query * string (* query AS name *)

     and query =
         Query1 of {Select : sitem list, From : fitem list, Where : sqexp option}
       | Union of query * query

val query : query parser

datatype dml =
         Insert of string * (string * sqexp) list
       | Delete of string * sqexp
       | Update of string * (string * sqexp) list * sqexp

val dml : dml parser

end
