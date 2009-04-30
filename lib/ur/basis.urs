type int
type float
type string
type time
type blob

type unit = {}

datatype bool = False | True

datatype option t = None | Some of t

datatype list t = Nil | Cons of t * list t


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


(** * Monads *)

class monad :: (Type -> Type) -> Type
val return : m ::: (Type -> Type) -> t ::: Type
             -> monad m
             -> t -> m t
val bind : m ::: (Type -> Type) -> t1 ::: Type -> t2 ::: Type
           -> monad m
           -> m t1 -> (t1 -> m t2)
           -> m t2

con transaction :: Type -> Type
val transaction_monad : monad transaction

con source :: Type -> Type
val source : t ::: Type -> t -> transaction (source t)
val set : t ::: Type -> source t -> t -> transaction unit
val get : t ::: Type -> source t -> transaction t

con signal :: Type -> Type
val signal_monad : monad signal
val signal : t ::: Type -> source t -> signal t


(** HTTP operations *)

val requestHeader : string -> transaction (option string)

con http_cookie :: Type -> Type
val getCookie : t ::: Type -> http_cookie t -> transaction (option t)
val setCookie : t ::: Type -> http_cookie t -> t -> transaction unit


(** JavaScript-y gadgets *)

val alert : string -> transaction unit
val spawn : transaction unit -> transaction unit
val sleep : int -> transaction unit


(** Channels *)

con channel :: Type -> Type
val channel : t ::: Type -> transaction (channel t)
val send : t ::: Type -> channel t -> t -> transaction unit
val recv : t ::: Type -> channel t -> transaction t

type client
val self : transaction client


(** SQL *)

con sql_table :: {Type} -> {{Unit}} -> Type
con sql_view :: {Type} -> Type

class fieldsOf :: Type -> {Type} -> Type
val fieldsOf_table : fs ::: {Type} -> keys ::: {{Unit}}
                     -> fieldsOf (sql_table fs keys) fs
val fieldsOf_view : fs ::: {Type}
                    -> fieldsOf (sql_view fs) fs

(*** Constraints *)

(**** Primary keys *)

class sql_injectable_prim
val sql_bool : sql_injectable_prim bool
val sql_int : sql_injectable_prim int
val sql_float : sql_injectable_prim float
val sql_string : sql_injectable_prim string
val sql_time : sql_injectable_prim time
val sql_blob : sql_injectable_prim blob
val sql_channel : t ::: Type -> sql_injectable_prim (channel t)
val sql_client : sql_injectable_prim client

con primary_key :: {Type} -> {{Unit}} -> Type
val no_primary_key : fs ::: {Type} -> primary_key fs []
val primary_key : rest ::: {Type} -> t ::: Type -> key1 :: Name -> keys :: {Type}
                  -> [[key1] ~ keys] => [[key1 = t] ++ keys ~ rest]
    => $([key1 = sql_injectable_prim t] ++ map sql_injectable_prim keys)
       -> primary_key ([key1 = t] ++ keys ++ rest)
          [Pkey = [key1] ++ map (fn _ => ()) keys]

(**** Other constraints *)

con sql_constraints :: {Type} -> {{Unit}} -> Type
(* Arguments: column types, uniqueness implications of constraints *)

con sql_constraint :: {Type} -> {Unit} -> Type

val no_constraint : fs ::: {Type} -> sql_constraints fs []
val one_constraint : fs ::: {Type} -> unique ::: {Unit} -> name :: Name
                     -> sql_constraint fs unique
                     -> sql_constraints fs [name = unique]
val join_constraints : fs ::: {Type}
                       -> uniques1 ::: {{Unit}} -> uniques2 ::: {{Unit}} -> [uniques1 ~ uniques2]
    => sql_constraints fs uniques1 -> sql_constraints fs uniques2
       -> sql_constraints fs (uniques1 ++ uniques2)


val unique : rest ::: {Type} -> t ::: Type -> unique1 :: Name -> unique :: {Type}
             -> [[unique1] ~ unique] => [[unique1 = t] ++ unique ~ rest]
    => sql_constraint ([unique1 = t] ++ unique ++ rest) ([unique1] ++ map (fn _ => ()) unique)

class linkable :: Type -> Type -> Type
val linkable_same : t ::: Type -> linkable t t
val linkable_from_nullable : t ::: Type -> linkable (option t) t
val linkable_to_nullable : t ::: Type -> linkable t (option t)

con matching :: {Type} -> {Type} -> Type
val mat_nil : matching [] []
val mat_cons : t1 ::: Type -> rest1 ::: {Type} -> t2 ::: Type -> rest2 ::: {Type}
               -> nm1 :: Name -> nm2 :: Name
               -> [[nm1] ~ rest1] => [[nm2] ~ rest2]
    => linkable t1 t2
       -> matching rest1 rest2
       -> matching ([nm1 = t1] ++ rest1) ([nm2 = t2] ++ rest2)

con propagation_mode :: {Type} -> Type
val restrict : fs ::: {Type} -> propagation_mode fs
val cascade : fs ::: {Type} -> propagation_mode fs
val no_action : fs ::: {Type} -> propagation_mode fs
val set_null : fs ::: {Type} -> propagation_mode (map option fs)


val foreign_key : mine1 ::: Name -> t ::: Type -> mine ::: {Type} -> munused ::: {Type}
                  -> foreign ::: {Type} -> funused ::: {Type}
                  -> nm ::: Name -> uniques ::: {{Unit}}
                  -> [[mine1] ~ mine] => [[mine1 = t] ++ mine ~ munused]
    => [foreign ~ funused] => [[nm] ~ uniques]
    => matching ([mine1 = t] ++ mine) foreign
       -> sql_table (foreign ++ funused) ([nm = map (fn _ => ()) foreign] ++ uniques)
       -> {OnDelete : propagation_mode ([mine1 = t] ++ mine),
           OnUpdate : propagation_mode ([mine1 = t] ++ mine)}
       -> sql_constraint ([mine1 = t] ++ mine ++ munused) []

con sql_exp :: {{Type}} -> {{Type}} -> {Type} -> Type -> Type

val check : fs ::: {Type}
            -> sql_exp [] [] fs bool
            -> sql_constraint fs []



(*** Queries *)

con sql_query :: {{Type}} -> {Type} -> Type
con sql_query1 :: {{Type}} -> {{Type}} -> {Type} -> Type

con sql_subset :: {{Type}} -> {{Type}} -> Type
val sql_subset : keep_drop :: {({Type} * {Type})}
                              -> sql_subset
                                     (map (fn fields :: ({Type} * {Type}) => fields.1 ++ fields.2) keep_drop)
                                     (map (fn fields :: ({Type} * {Type}) => fields.1) keep_drop)
val sql_subset_all : tables :: {{Type}} -> sql_subset tables tables

con sql_from_items :: {{Type}} -> Type

val sql_from_table : t ::: Type -> fs ::: {Type}
                     -> fieldsOf t fs -> name :: Name
                     -> t -> sql_from_items [name = fs]
val sql_from_comma : tabs1 ::: {{Type}} -> tabs2 ::: {{Type}}
                     -> [tabs1 ~ tabs2]
    => sql_from_items tabs1 -> sql_from_items tabs2
       -> sql_from_items (tabs1 ++ tabs2)
val sql_inner_join : tabs1 ::: {{Type}} -> tabs2 ::: {{Type}}
                     -> [tabs1 ~ tabs2]
    => sql_from_items tabs1 -> sql_from_items tabs2
       -> sql_exp (tabs1 ++ tabs2) [] [] bool
       -> sql_from_items (tabs1 ++ tabs2)

class nullify :: Type -> Type -> Type
val nullify_option : t ::: Type -> nullify (option t) (option t)
val nullify_prim : t ::: Type -> sql_injectable_prim t -> nullify t (option t)

val sql_left_join : tabs1 ::: {{Type}} -> tabs2 ::: {{(Type * Type)}}
                     -> [tabs1 ~ tabs2]
    => $(map (fn r => $(map (fn p :: (Type * Type) => nullify p.1 p.2) r)) tabs2)
       -> sql_from_items tabs1 -> sql_from_items (map (map (fn p :: (Type * Type) => p.1)) tabs2)
       -> sql_exp (tabs1 ++ map (map (fn p :: (Type * Type) => p.1)) tabs2) [] [] bool
       -> sql_from_items (tabs1 ++ map (map (fn p :: (Type * Type) => p.2)) tabs2)

val sql_right_join : tabs1 ::: {{(Type * Type)}} -> tabs2 ::: {{Type}}
                     -> [tabs1 ~ tabs2]
    => $(map (fn r => $(map (fn p :: (Type * Type) => nullify p.1 p.2) r)) tabs1)
       -> sql_from_items (map (map (fn p :: (Type * Type) => p.1)) tabs1) -> sql_from_items tabs2
       -> sql_exp (map (map (fn p :: (Type * Type) => p.1)) tabs1 ++ tabs2) [] [] bool
       -> sql_from_items (map (map (fn p :: (Type * Type) => p.2)) tabs1 ++ tabs2)

val sql_full_join : tabs1 ::: {{(Type * Type)}} -> tabs2 ::: {{(Type * Type)}}
                     -> [tabs1 ~ tabs2]
    => $(map (fn r => $(map (fn p :: (Type * Type) => nullify p.1 p.2) r)) (tabs1 ++ tabs2))
       -> sql_from_items (map (map (fn p :: (Type * Type) => p.1)) tabs1)
       -> sql_from_items (map (map (fn p :: (Type * Type) => p.1)) tabs2)
       -> sql_exp (map (map (fn p :: (Type * Type) => p.1)) (tabs1 ++ tabs2)) [] [] bool
       -> sql_from_items (map (map (fn p :: (Type * Type) => p.2)) (tabs1 ++ tabs2))

val sql_query1 : tables ::: {{Type}}
                 -> grouped ::: {{Type}}
                 -> selectedFields ::: {{Type}}
                 -> selectedExps ::: {Type}
                 -> {From : sql_from_items tables,
                     Where : sql_exp tables [] [] bool,
                     GroupBy : sql_subset tables grouped,
                     Having : sql_exp grouped tables [] bool,
                     SelectFields : sql_subset grouped selectedFields,
                     SelectExps : $(map (sql_exp grouped tables [])
                                            selectedExps) }
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
val sql_prim : t ::: Type -> sql_injectable_prim t -> sql_injectable t
val sql_option_prim : t ::: Type -> sql_injectable_prim t -> sql_injectable (option t)

val sql_inject : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                 -> t ::: Type
                 -> sql_injectable t -> t -> sql_exp tables agg exps t

val sql_is_null : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                  -> t ::: Type
                  -> sql_exp tables agg exps (option t)
                  -> sql_exp tables agg exps bool

class sql_arith
val sql_int_arith : sql_arith int
val sql_float_arith : sql_arith float

con sql_unary :: Type -> Type -> Type
val sql_not : sql_unary bool bool
val sql_unary : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                -> arg ::: Type -> res ::: Type
                -> sql_unary arg res -> sql_exp tables agg exps arg
                -> sql_exp tables agg exps res

val sql_neg : t ::: Type -> sql_arith t -> sql_unary t t

con sql_binary :: Type -> Type -> Type -> Type
val sql_and : sql_binary bool bool bool
val sql_or : sql_binary bool bool bool
val sql_binary : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                 -> arg1 ::: Type -> arg2 ::: Type -> res ::: Type
                 -> sql_binary arg1 arg2 res -> sql_exp tables agg exps arg1
                 -> sql_exp tables agg exps arg2
                 -> sql_exp tables agg exps res

val sql_plus : t ::: Type -> sql_arith t -> sql_binary t t t
val sql_minus : t ::: Type -> sql_arith t -> sql_binary t t t
val sql_times : t ::: Type -> sql_arith t -> sql_binary t t t
val sql_div : t ::: Type -> sql_arith t -> sql_binary t t t
val sql_mod : sql_binary int int int

val sql_eq : t ::: Type -> sql_binary t t bool
val sql_ne : t ::: Type -> sql_binary t t bool
val sql_lt : t ::: Type -> sql_binary t t bool
val sql_le : t ::: Type -> sql_binary t t bool
val sql_gt : t ::: Type -> sql_binary t t bool
val sql_ge : t ::: Type -> sql_binary t t bool

val sql_count : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                -> sql_exp tables agg exps int

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

con sql_ufunc :: Type -> Type -> Type
val sql_ufunc : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                -> dom ::: Type -> ran ::: Type
                -> sql_ufunc dom ran -> sql_exp tables agg exps dom
                -> sql_exp tables agg exps ran
val sql_octet_length : sql_ufunc blob int


(*** Executing queries *)

val query : tables ::: {{Type}} -> exps ::: {Type}
            -> [tables ~ exps] =>
                  state ::: Type
                  -> sql_query tables exps
                  -> ($(exps ++ map (fn fields :: {Type} => $fields) tables)
                      -> state
                      -> transaction state)
                  -> state
                  -> transaction state


(*** Database mutators *)

type dml
val dml : dml -> transaction unit

val insert : fields ::: {Type} -> uniques ::: {{Unit}}
             -> sql_table fields uniques
             -> $(map (fn t :: Type => sql_exp [] [] [] t) fields)
             -> dml

val update : unchanged ::: {Type} -> uniques ::: {{Unit}} -> changed :: {Type} ->
             [changed ~ unchanged] =>
                $(map (fn t :: Type => sql_exp [T = changed ++ unchanged] [] [] t) changed)
                -> sql_table (changed ++ unchanged) uniques
                -> sql_exp [T = changed ++ unchanged] [] [] bool
                -> dml

val delete : fields ::: {Type} -> uniques ::: {{Unit}}
             -> sql_table fields uniques
             -> sql_exp [T = fields] [] [] bool
             -> dml

(*** Sequences *)

type sql_sequence
val nextval : sql_sequence -> transaction int


(** XML *)

type css_class

con tag :: {Type} -> {Unit} -> {Unit} -> {Type} -> {Type} -> Type

con xml :: {Unit} -> {Type} -> {Type} -> Type
val cdata : ctx ::: {Unit} -> use ::: {Type} -> string -> xml ctx use []
val tag : attrsGiven ::: {Type} -> attrsAbsent ::: {Type}
          -> ctxOuter ::: {Unit} -> ctxInner ::: {Unit}
          -> useOuter ::: {Type} -> useInner ::: {Type}
          -> bindOuter ::: {Type} -> bindInner ::: {Type}
          -> [attrsGiven ~ attrsAbsent] =>
             [useOuter ~ useInner] =>
             [bindOuter ~ bindInner] =>
           option css_class
           -> $attrsGiven
           -> tag (attrsGiven ++ attrsAbsent)
                  ctxOuter ctxInner useOuter bindOuter
           -> xml ctxInner useInner bindInner
           -> xml ctxOuter (useOuter ++ useInner) (bindOuter ++ bindInner)
val join : ctx ::: {Unit} 
        -> use1 ::: {Type} -> bind1 ::: {Type} -> bind2 ::: {Type}
        -> [use1 ~ bind1] => [bind1 ~ bind2] =>
              xml ctx use1 bind1
              -> xml ctx (use1 ++ bind1) bind2
              -> xml ctx use1 (bind1 ++ bind2)
val useMore : ctx ::: {Unit} -> use1 ::: {Type} -> use2 ::: {Type}
              -> bind ::: {Type}
              -> [use1 ~ use2] =>
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

type url
val bless : string -> url

val dyn : use ::: {Type} -> bind ::: {Type} -> unit
          -> tag [Signal = signal (xml body use bind)] body [] use bind

val head : unit -> tag [] html head [] []
val title : unit -> tag [] head [] [] []
val link : unit -> tag [Rel = string, Typ = string, Href = url, Media = string] head [] [] []

val body : unit -> tag [Onload = transaction unit] html body [] []
con bodyTag = fn (attrs :: {Type}) =>
                 ctx ::: {Unit} ->
                 [[Body] ~ ctx] =>
                    unit -> tag attrs ([Body] ++ ctx) ([Body] ++ ctx) [] []
con bodyTagStandalone = fn (attrs :: {Type}) =>
                           ctx ::: {Unit}
                           -> [[Body] ~ ctx] =>
                                 unit -> tag attrs ([Body] ++ ctx) [] [] []

val br : bodyTagStandalone []

val span : bodyTag []

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
val ol : bodyTag []
val ul : bodyTag []

val hr : bodyTag []

val a : bodyTag [Link = transaction page, Href = url, Onclick = transaction unit]

val img : bodyTag [Src = url]

val form : ctx ::: {Unit} -> bind ::: {Type}
           -> [[Body] ~ ctx] =>
    xml form [] bind
    -> xml ([Body] ++ ctx) [] []
       
val subform : ctx ::: {Unit} -> use ::: {Type} -> bind ::: {Type}
              -> [[Form] ~ ctx] =>
    nm :: Name -> [[nm] ~ use] =>
    xml form [] bind
    -> xml ([Form] ++ ctx) use [nm = $bind]
       
con formTag = fn (ty :: Type) (inner :: {Unit}) (attrs :: {Type}) =>
                  ctx ::: {Unit}
                  -> [[Form] ~ ctx] =>
                        nm :: Name -> unit
                        -> tag attrs ([Form] ++ ctx) inner [] [nm = ty]
val textbox : formTag string [] [Value = string, Size = int, Source = source string]
val password : formTag string [] [Value = string, Size = int]
val textarea : formTag string [] [Rows = int, Cols = int]

val checkbox : formTag bool [] [Checked = bool]

type file
val fileName : file -> option string
val fileMimeType : file -> string
val fileData : file -> blob

val upload : formTag file [] [Value = string, Size = int]

type mimeType
val blessMime : string -> mimeType
val returnBlob : t ::: Type -> blob -> mimeType -> transaction t
val blobSize : blob -> int

con radio = [Body, Radio]
val radio : formTag string radio []
val radioOption : unit -> tag [Value = string] radio [] [] []

con select = [Select]
val select : formTag string select []
val option : unit -> tag [Value = string, Selected = bool] select [] [] []

val submit : ctx ::: {Unit} -> use ::: {Type}
             -> [[Form] ~ ctx] =>
                   unit
                   -> tag [Value = string, Action = $use -> transaction page]
                          ([Form] ++ ctx) ([Form] ++ ctx) use []

(*** AJAX-oriented widgets *)

con cformTag = fn (attrs :: {Type}) =>
                  ctx ::: {Unit}
                  -> [[Body] ~ ctx] =>
                        unit -> tag attrs ([Body] ++ ctx) [] [] []

val ctextbox : cformTag [Value = string, Size = int, Source = source string]
val button : cformTag [Value = string, Onclick = transaction unit]

(*** Tables *)

val tabl : other ::: {Unit} -> [other ~ [Body, Table]] =>
                                  unit -> tag [Border = int] ([Body] ++ other) ([Body, Table] ++ other) [] []
val tr : other ::: {Unit} -> [other ~ [Body, Table, Tr]] =>
                                unit -> tag [] ([Body, Table] ++ other) ([Body, Tr] ++ other) [] []
val th : other ::: {Unit} -> [other ~ [Body, Tr]] =>
                                unit -> tag [] ([Body, Tr] ++ other) ([Body] ++ other) [] []
val td : other ::: {Unit} -> [other ~ [Body, Tr]] =>
                                unit -> tag [] ([Body, Tr] ++ other) ([Body] ++ other) [] []


(** Aborting *)

val error : t ::: Type -> xbody -> t

(* Client-side-only handlers: *)
val onError : (xbody -> transaction unit) -> transaction unit
val onFail : (string -> transaction unit) -> transaction unit
val onConnectFail : transaction unit -> transaction unit
val onDisconnect : transaction unit -> transaction unit
val onServerError : (string -> transaction unit) -> transaction unit

val show_xml : ctx ::: {Unit} -> use ::: {Type} -> bind ::: {Type} -> show (xml ctx use bind)
