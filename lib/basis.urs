type int
type float
type string
type time

type unit = {}

datatype bool = False | True

datatype option t = None | Some of t


(** Basic type classes *)

class eq
val eq : t ::: Type -> eq t -> t -> t -> bool
val ne : t ::: Type -> eq t -> t -> t -> bool
val eq_int : eq int
val eq_float : eq float
val eq_string : eq string
val eq_bool : eq bool
val eq_time : eq time
val mkEq : t ::: Type -> (t -> t -> bool) -> eq t

class num
val zero : t ::: Type -> num t -> t
val neg : t ::: Type -> num t -> t -> t
val plus : t ::: Type -> num t -> t -> t -> t
val minus : t ::: Type -> num t -> t -> t -> t
val times : t ::: Type -> num t -> t -> t -> t
val div : t ::: Type -> num t -> t -> t -> t
val mod : t ::: Type -> num t -> t -> t -> t
val num_int : num int
val num_float : num float

class ord
val lt : t ::: Type -> ord t -> t -> t -> bool
val le : t ::: Type -> ord t -> t -> t -> bool
val gt : t ::: Type -> ord t -> t -> t -> bool
val ge : t ::: Type -> ord t -> t -> t -> bool
val ord_int : ord int
val ord_float : ord float
val ord_string : ord string
val ord_bool : ord bool
val ord_time : ord time


(** String operations *)

val strcat : string -> string -> string

class show
val show : t ::: Type -> show t -> t -> string
val show_int : show int
val show_float : show float
val show_string : show string
val show_bool : show bool
val show_time : show time
val mkShow : t ::: Type -> (t -> string) -> show t

class read
val read : t ::: Type -> read t -> string -> option t
val readError : t ::: Type -> read t -> string -> t
(* [readError] calls [error] if the input is malformed. *)
val read_int : read int
val read_float : read float
val read_string : read string
val read_bool : read bool
val read_time : read time


(** * Transactions *)

con transaction :: Type -> Type

val return : t ::: Type
             -> t -> transaction t
val bind : t1 ::: Type -> t2 ::: Type
           -> transaction t1 -> (t1 -> transaction t2)
           -> transaction t2


(** HTTP operations *)

val requestHeader : string -> transaction (option string)

con http_cookie :: Type -> Type
val getCookie : t ::: Type -> http_cookie t -> transaction (option t)
val setCookie : t ::: Type -> http_cookie t -> t -> transaction unit


(** SQL *)

con sql_table :: {Type} -> Type

(*** Queries *)

con sql_query :: {{Type}} -> {Type} -> Type
con sql_query1 :: {{Type}} -> {{Type}} -> {Type} -> Type
con sql_exp :: {{Type}} -> {{Type}} -> {Type} -> Type -> Type

con sql_subset :: {{Type}} -> {{Type}} -> Type
val sql_subset : keep_drop :: {({Type} * {Type})}
                              -> sql_subset
                                     (fold (fn nm (fields :: ({Type} * {Type}))
                                                  acc [[nm] ~ acc]
                                                  [fields.1 ~ fields.2] =>
                                               [nm = fields.1 ++ fields.2]
                                                   ++ acc) [] keep_drop)
                                     (fold (fn nm (fields :: ({Type} * {Type}))
                                                  acc [[nm] ~ acc] =>
                                               [nm = fields.1] ++ acc)
                                               [] keep_drop)
val sql_subset_all : tables :: {{Type}} -> sql_subset tables tables

val sql_query1 : tables ::: {{Type}}
                 -> grouped ::: {{Type}}
                 -> selectedFields ::: {{Type}}
                 -> selectedExps ::: {Type}
                 -> {From : $(fold (fn nm (fields :: {Type}) acc [[nm] ~ acc] =>
                                       [nm = sql_table fields] ++ acc)
                                       [] tables),
                     Where : sql_exp tables [] [] bool,
                     GroupBy : sql_subset tables grouped,
                     Having : sql_exp grouped tables [] bool,
                     SelectFields : sql_subset grouped selectedFields,
                     SelectExps : $(fold (fn nm (t :: Type) acc [[nm] ~ acc] =>
                                             [nm = sql_exp grouped tables [] t]
                                                 ++ acc) [] selectedExps) }
                 -> sql_query1 tables selectedFields selectedExps

type sql_relop 
val sql_union : sql_relop
val sql_intersect : sql_relop
val sql_except : sql_relop
val sql_relop : tables1 ::: {{Type}}
                -> tables2 ::: {{Type}}
                -> selectedFields ::: {{Type}}
                -> selectedExps ::: {Type}
                -> sql_relop
                -> sql_query1 tables1 selectedFields selectedExps
                -> sql_query1 tables2 selectedFields selectedExps
                -> sql_query1 selectedFields selectedFields selectedExps

type sql_direction
val sql_asc : sql_direction
val sql_desc : sql_direction

con sql_order_by :: {{Type}} -> {Type} -> Type
val sql_order_by_Nil : tables ::: {{Type}} -> exps :: {Type} -> sql_order_by tables exps
val sql_order_by_Cons : tables ::: {{Type}} -> exps ::: {Type} -> t ::: Type
                        -> sql_exp tables [] exps t -> sql_direction
                        -> sql_order_by tables exps
                        -> sql_order_by tables exps

type sql_limit
val sql_no_limit : sql_limit
val sql_limit : int -> sql_limit
                       
type sql_offset
val sql_no_offset : sql_offset
val sql_offset : int -> sql_offset

val sql_query : tables ::: {{Type}}
                -> selectedFields ::: {{Type}}
                -> selectedExps ::: {Type}
                -> {Rows : sql_query1 tables selectedFields selectedExps,
                    OrderBy : sql_order_by tables selectedExps,
                    Limit : sql_limit,
                    Offset : sql_offset}
                -> sql_query selectedFields selectedExps

val sql_field : otherTabs ::: {{Type}} -> otherFields ::: {Type}
                -> fieldType ::: Type -> agg ::: {{Type}}
                -> exps ::: {Type}
                -> tab :: Name -> field :: Name
                -> sql_exp
                       ([tab = [field = fieldType] ++ otherFields] ++ otherTabs)
                       agg exps fieldType

val sql_exp : tabs ::: {{Type}} -> agg ::: {{Type}} -> t ::: Type -> rest ::: {Type}
              -> nm :: Name
              -> sql_exp tabs agg ([nm = t] ++ rest) t

class sql_injectable
val sql_bool : sql_injectable bool
val sql_int : sql_injectable int
val sql_float : sql_injectable float
val sql_string : sql_injectable string
val sql_time : sql_injectable time
val sql_inject : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                 -> t ::: Type
                 -> sql_injectable t -> t -> sql_exp tables agg exps t

con sql_unary :: Type -> Type -> Type
val sql_not : sql_unary bool bool
val sql_unary : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                -> arg ::: Type -> res ::: Type
                -> sql_unary arg res -> sql_exp tables agg exps arg
                -> sql_exp tables agg exps res

con sql_binary :: Type -> Type -> Type -> Type
val sql_and : sql_binary bool bool bool
val sql_or : sql_binary bool bool bool
val sql_binary : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                 -> arg1 ::: Type -> arg2 ::: Type -> res ::: Type
                 -> sql_binary arg1 arg2 res -> sql_exp tables agg exps arg1
                 -> sql_exp tables agg exps arg2
                 -> sql_exp tables agg exps res

type sql_comparison
val sql_eq : sql_comparison
val sql_ne : sql_comparison
val sql_lt : sql_comparison
val sql_le : sql_comparison
val sql_gt : sql_comparison
val sql_ge : sql_comparison
val sql_comparison : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
        -> t ::: Type
        -> sql_comparison
        -> sql_exp tables agg exps t -> sql_exp tables agg exps t
        -> sql_exp tables agg exps bool

val sql_count : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                -> unit -> sql_exp tables agg exps int

con sql_aggregate :: Type -> Type
val sql_aggregate : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                    -> t ::: Type
                    -> sql_aggregate t -> sql_exp agg agg exps t
                    -> sql_exp tables agg exps t

class sql_summable
val sql_summable_int : sql_summable int
val sql_summable_float : sql_summable float
val sql_avg : t ::: Type -> sql_summable t -> sql_aggregate t
val sql_sum : t ::: Type -> sql_summable t -> sql_aggregate t

class sql_maxable
val sql_maxable_int : sql_maxable int
val sql_maxable_float : sql_maxable float
val sql_maxable_string : sql_maxable string
val sql_maxable_time : sql_maxable time
val sql_max : t ::: Type -> sql_maxable t -> sql_aggregate t
val sql_min : t ::: Type -> sql_maxable t -> sql_aggregate t

con sql_nfunc :: Type -> Type
val sql_nfunc : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                -> t ::: Type
                -> sql_nfunc t -> sql_exp tables agg exps t
val sql_current_timestamp : sql_nfunc time


(*** Executing queries *)

val query : tables ::: {{Type}} -> exps ::: {Type}
            -> fn [tables ~ exps] =>
                  state ::: Type
                  -> sql_query tables exps
                  -> ($(exps ++ fold (fn nm (fields :: {Type}) acc [[nm] ~ acc] =>
                                         [nm = $fields] ++ acc) [] tables)
                      -> state
                      -> transaction state)
                  -> state
                  -> transaction state


(*** Database mutators *)

type dml
val dml : dml -> transaction unit

val insert : fields ::: {Type}
             -> sql_table fields
             -> $(fold (fn nm (t :: Type) acc [[nm] ~ acc] =>
                           [nm = sql_exp [] [] [] t] ++ acc)
                           [] fields)
             -> dml

val update : unchanged ::: {Type} -> changed :: {Type} ->
             fn [changed ~ unchanged] =>
                $(fold (fn nm (t :: Type) acc [[nm] ~ acc] =>
                           [nm = sql_exp [T = changed ++ unchanged] [] [] t]
                               ++ acc)
                           [] changed)
                -> sql_table (changed ++ unchanged)
                -> sql_exp [T = changed ++ unchanged] [] [] bool
                -> dml

val delete : fields ::: {Type}
             -> sql_table fields
             -> sql_exp [T = fields] [] [] bool
             -> dml

(*** Sequences *)

type sql_sequence
val nextval : sql_sequence -> transaction int


(** XML *)

con tag :: {Type} -> {Unit} -> {Unit} -> {Type} -> {Type} -> Type


con xml :: {Unit} -> {Type} -> {Type} -> Type
val cdata : ctx ::: {Unit} -> use ::: {Type} -> string -> xml ctx use []
val tag : attrsGiven ::: {Type} -> attrsAbsent ::: {Type}
          -> ctxOuter ::: {Unit} -> ctxInner ::: {Unit}
          -> useOuter ::: {Type} -> useInner ::: {Type}
          -> bindOuter ::: {Type} -> bindInner ::: {Type}
          -> fn [attrsGiven ~ attrsAbsent]
                    [useOuter ~ useInner]
                    [bindOuter ~ bindInner] =>
                $attrsGiven
                -> tag (attrsGiven ++ attrsAbsent)
                       ctxOuter ctxInner useOuter bindOuter
                -> xml ctxInner useInner bindInner
                -> xml ctxOuter (useOuter ++ useInner) (bindOuter ++ bindInner)
val join : ctx ::: {Unit} 
        -> use1 ::: {Type} -> bind1 ::: {Type} -> bind2 ::: {Type}
        -> fn [use1 ~ bind1] [bind1 ~ bind2] =>
              xml ctx use1 bind1
              -> xml ctx (use1 ++ bind1) bind2
              -> xml ctx use1 (bind1 ++ bind2)
val useMore : ctx ::: {Unit} -> use1 ::: {Type} -> use2 ::: {Type}
              -> bind ::: {Type}
              -> fn [use1 ~ use2] =>
                    xml ctx use1 bind
                    -> xml ctx (use1 ++ use2) bind

con xhtml = xml [Html]
con page = xhtml [] []
con xbody = xml [Body] [] []
con xtr = xml [Body, Tr] [] []
con xform = xml [Body, Form] [] []

(*** HTML details *)

con html = [Html]
con head = [Head]
con body = [Body]
con form = [Body, Form]
con tabl = [Body, Table]
con tr = [Body, Tr]

val head : unit -> tag [] html head [] []
val title : unit -> tag [] head [] [] []

val body : unit -> tag [] html body [] []
con bodyTag = fn (attrs :: {Type}) =>
                 ctx ::: {Unit} ->
                 fn [[Body] ~ ctx] =>
                    unit -> tag attrs ([Body] ++ ctx) ([Body] ++ ctx) [] []
con bodyTagStandalone = fn (attrs :: {Type}) =>
                           ctx ::: {Unit}
                           -> fn [[Body] ~ ctx] =>
                                 unit -> tag attrs ([Body] ++ ctx) [] [] []

val br : bodyTagStandalone []

val p : bodyTag []
val b : bodyTag []
val i : bodyTag []
val tt : bodyTag []
val font : bodyTag [Size = int, Face = string]

val h1 : bodyTag []
val h2 : bodyTag []
val h3 : bodyTag []
val h4 : bodyTag []
val li : bodyTag []

val hr : bodyTag []

val a : bodyTag [Link = transaction page]

val form : ctx ::: {Unit} -> bind ::: {Type}
            -> fn [[Body] ~ ctx] =>
                  xml form [] bind
                  -> xml ([Body] ++ ctx) [] []
con formTag = fn (ty :: Type) (inner :: {Unit}) (attrs :: {Type}) =>
                  ctx ::: {Unit}
                  -> fn [[Form] ~ ctx] =>
                        nm :: Name -> unit
                        -> tag attrs ([Form] ++ ctx) inner [] [nm = ty]
val textbox : formTag string [] [Value = string, Size = int]
val password : formTag string [] [Value = string, Size = int]
val textarea : formTag string [] [Rows = int, Cols = int]

val checkbox : formTag bool [] [Checked = bool]

con radio = [Body, Radio]
val radio : formTag string radio []
val radioOption : unit -> tag [Value = string] radio [] [] []

con select = [Select]
val select : formTag string select []
val option : unit -> tag [Value = string, Selected = bool] select [] [] []

val submit : ctx ::: {Unit} ->  use ::: {Type}
             -> fn [[Form] ~ ctx] =>
                   unit
                   -> tag [Value = string, Action = $use -> transaction page]
                          ([Form] ++ ctx) ([Form] ++ ctx) use []

(*** Tables *)

val tabl : other ::: {Unit} -> fn [other ~ [Body, Table]] =>
                                  unit -> tag [Border = int] ([Body] ++ other) ([Body, Table] ++ other) [] []
val tr : other ::: {Unit} -> fn [other ~ [Body, Table, Tr]] =>
                                unit -> tag [] ([Body, Table] ++ other) ([Body, Tr] ++ other) [] []
val th : other ::: {Unit} -> fn [other ~ [Body, Tr]] =>
                                unit -> tag [] ([Body, Tr] ++ other) ([Body] ++ other) [] []
val td : other ::: {Unit} -> fn [other ~ [Body, Tr]] =>
                                unit -> tag [] ([Body, Tr] ++ other) ([Body] ++ other) [] []


(** Aborting *)

val error : t ::: Type -> xml [Body] [] [] -> t
