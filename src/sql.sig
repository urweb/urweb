signature SQL = sig

val fu : Mono.file -> unit

val debug : bool ref

type lvar

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

datatype ('a, 'b) sum = inl of 'a | inr of 'b

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

datatype sitem =
         SqField of string * string
       | SqExp of sqexp * string

type query1

datatype query =
         Query1 of query1
       | Union of query * query

datatype dml =
         Insert of string * (string * sqexp) list
       | Delete of string * sqexp
       | Update of string * (string * sqexp) list * sqexp

end
