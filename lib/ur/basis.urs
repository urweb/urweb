type int
type float
type string
type char
type time
type blob

type unit = {}

datatype bool = False | True

datatype option t = None | Some of t

datatype list t = Nil | Cons of t * list t


(** Polymorphic variants *)

con variant :: {Type} -> Type
val make : nm :: Name -> t ::: Type -> ts ::: {Type} -> [[nm] ~ ts] => t -> variant ([nm = t] ++ ts)
val match : ts ::: {Type} -> t ::: Type -> variant ts -> $(map (fn t' => t' -> t) ts) -> t


(** Basic type classes *)

class eq
val eq : t ::: Type -> eq t -> t -> t -> bool
val ne : t ::: Type -> eq t -> t -> t -> bool
val eq_int : eq int
val eq_float : eq float
val eq_string : eq string
val eq_char : eq char
val eq_bool : eq bool
val eq_time : eq time
val mkEq : t ::: Type -> (t -> t -> bool) -> eq t

class num
val zero : t ::: Type -> num t -> t
val neg : t ::: Type -> num t -> t -> t
val plus : t ::: Type -> num t -> t -> t -> t
val minus : t ::: Type -> num t -> t -> t -> t
val times : t ::: Type -> num t -> t -> t -> t
val divide : t ::: Type -> num t -> t -> t -> t
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
val ord_char : ord char
val ord_bool : ord bool
val ord_time : ord time
val mkOrd : t ::: Type -> {Lt : t -> t -> bool, Le : t -> t -> bool} -> ord t


(** Character operations *)

val isalnum : char -> bool
val isalpha : char -> bool
val isblank : char -> bool
val iscntrl : char -> bool
val isdigit : char -> bool
val isgraph : char -> bool
val islower : char -> bool
val isprint : char -> bool
val ispunct : char -> bool
val isspace : char -> bool
val isupper : char -> bool
val isxdigit : char -> bool
val tolower : char -> char
val toupper : char -> char
val ord : char -> int
val chr : int -> char

(** String operations *)

val strlen : string -> int
val strlenGe : string -> int -> bool
val strcat : string -> string -> string
val strsub : string -> int -> char
val strsuffix : string -> int -> string
val strchr : string -> char -> option string
val strindex : string -> char -> option int
val strsindex : string -> string -> option int
val strcspn : string -> string -> int
val substring : string -> int -> int -> string
val str1 : char -> string

class show
val show : t ::: Type -> show t -> t -> string
val show_int : show int
val show_float : show float
val show_string : show string
val show_char : show char
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
val read_char : read char
val read_bool : read bool
val read_time : read time
val mkRead : t ::: Type -> (string -> t) -> (string -> option t) -> read t


(** * Monads *)

class monad :: (Type -> Type) -> Type
val return : m ::: (Type -> Type) -> t ::: Type
             -> monad m
             -> t -> m t
val bind : m ::: (Type -> Type) -> t1 ::: Type -> t2 ::: Type
           -> monad m
           -> m t1 -> (t1 -> m t2)
           -> m t2

val mkMonad : m ::: (Type -> Type)
              -> {Return : t ::: Type -> t -> m t,
                  Bind : t1 ::: Type -> t2 ::: Type -> m t1 -> (t1 -> m t2) -> m t2}
              -> monad m

con transaction :: Type -> Type
val transaction_monad : monad transaction

con source :: Type -> Type
val source : t ::: Type -> t -> transaction (source t)
val set : t ::: Type -> source t -> t -> transaction unit
val get : t ::: Type -> source t -> transaction t

con signal :: Type -> Type
val signal_monad : monad signal
val signal : t ::: Type -> source t -> signal t
val current : t ::: Type -> signal t -> transaction t


(** * Time *)

val now : transaction time
val minTime : time
val addSeconds : time -> int -> time
val toSeconds : time -> int
val diffInSeconds : time -> time -> int
(* Earlier time first *)
val timef : string -> time -> string (* Uses strftime() format string *)
val readUtc : string -> option time


(** * Encryption *)

val crypt : string -> string -> string


(** HTTP operations *)

con http_cookie :: Type -> Type
val getCookie : t ::: Type -> http_cookie t -> transaction (option t)
val setCookie : t ::: Type -> http_cookie t -> {Value : t,
                                                Expires : option time,
                                                Secure : bool} -> transaction unit
val clearCookie : t ::: Type -> http_cookie t -> transaction unit

type requestHeader
val blessRequestHeader : string -> requestHeader
val checkRequestHeader : string -> option requestHeader
val getHeader : requestHeader -> transaction (option string)

type responseHeader
val blessResponseHeader : string -> responseHeader
val checkResponseHeader : string -> option responseHeader
val setHeader : responseHeader -> string -> transaction unit


(** JavaScript-y gadgets *)

val alert : string -> transaction unit
val confirm : string -> transaction bool
val spawn : transaction unit -> transaction unit
val sleep : int -> transaction unit

val rpc : t ::: Type -> transaction t -> transaction t


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
val sql_char : sql_injectable_prim char
val sql_time : sql_injectable_prim time
val sql_blob : sql_injectable_prim blob
val sql_channel : t ::: Type -> sql_injectable_prim (channel t)
val sql_client : sql_injectable_prim client

con serialized :: Type -> Type
val serialize : t ::: Type -> t -> serialized t
val deserialize : t ::: Type -> serialized t -> t
val sql_serialized : t ::: Type -> sql_injectable_prim (serialized t)

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
val sql_exp_weaken : fs ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} -> t ::: Type
                     -> fs' ::: {{Type}} -> agg' ::: {{Type}} -> exps' ::: {Type}
                     -> [fs ~ fs'] => [agg ~ agg'] => [exps ~ exps'] =>
                     sql_exp fs agg exps t
                     -> sql_exp (fs ++ fs') (agg ++ agg') (exps ++ exps') t

val check : fs ::: {Type}
            -> sql_exp [] [] fs bool
            -> sql_constraint fs []


(*** Queries *)

con sql_query :: {{Type}} -> {{Type}} -> {{Type}} -> {Type} -> Type
con sql_query1 :: {{Type}} -> {{Type}} -> {{Type}} -> {{Type}} -> {Type} -> Type

con sql_subset :: {{Type}} -> {{Type}} -> Type
val sql_subset : keep_drop :: {({Type} * {Type})}
                              -> sql_subset
                                     (map (fn fields :: ({Type} * {Type}) => fields.1 ++ fields.2) keep_drop)
                                     (map (fn fields :: ({Type} * {Type}) => fields.1) keep_drop)
val sql_subset_all : tables :: {{Type}} -> sql_subset tables tables
val sql_subset_concat : big1 ::: {{Type}} -> little1 ::: {{Type}}
                        -> big2 ::: {{Type}} -> little2 ::: {{Type}}
                        -> [big1 ~ big2] => [little1 ~ little2] =>
    sql_subset big1 little1
    -> sql_subset big2 little2
    -> sql_subset (big1 ++ big2) (little1 ++ little2)

con sql_from_items :: {{Type}} -> {{Type}} -> Type

val sql_from_nil : free ::: {{Type}} -> sql_from_items free []
val sql_from_table : free ::: {{Type}} -> t ::: Type -> fs ::: {Type}
                     -> fieldsOf t fs -> name :: Name
                     -> t -> sql_from_items free [name = fs]
val sql_from_query : free ::: {{Type}} -> fs ::: {Type} -> name :: Name
                     -> sql_query free [] [] fs
                     -> sql_from_items free [name = fs]
val sql_from_comma : free ::: {{Type}} -> tabs1 ::: {{Type}} -> tabs2 ::: {{Type}}
                     -> [tabs1 ~ tabs2]
    => sql_from_items free tabs1 -> sql_from_items free tabs2
       -> sql_from_items free (tabs1 ++ tabs2)
val sql_inner_join : free ::: {{Type}} -> tabs1 ::: {{Type}} -> tabs2 ::: {{Type}}
                     -> [free ~ tabs1] => [free ~ tabs2] => [tabs1 ~ tabs2]
    => sql_from_items free tabs1 -> sql_from_items free tabs2
       -> sql_exp (free ++ tabs1 ++ tabs2) [] [] bool
       -> sql_from_items free (tabs1 ++ tabs2)

class nullify :: Type -> Type -> Type
val nullify_option : t ::: Type -> nullify (option t) (option t)
val nullify_prim : t ::: Type -> sql_injectable_prim t -> nullify t (option t)

val sql_left_join : free ::: {{Type}} -> tabs1 ::: {{Type}} -> tabs2 ::: {{(Type * Type)}}
                     -> [free ~ tabs1] => [free ~ tabs2] => [tabs1 ~ tabs2]
    => $(map (fn r => $(map (fn p :: (Type * Type) => nullify p.1 p.2) r)) tabs2)
       -> sql_from_items free tabs1 -> sql_from_items free (map (map (fn p :: (Type * Type) => p.1)) tabs2)
       -> sql_exp (free ++ tabs1 ++ map (map (fn p :: (Type * Type) => p.1)) tabs2) [] [] bool
       -> sql_from_items free (tabs1 ++ map (map (fn p :: (Type * Type) => p.2)) tabs2)

val sql_right_join : free ::: {{Type}} -> tabs1 ::: {{(Type * Type)}} -> tabs2 ::: {{Type}}
                     -> [free ~ tabs1] => [free ~ tabs2] => [tabs1 ~ tabs2]
    => $(map (fn r => $(map (fn p :: (Type * Type) => nullify p.1 p.2) r)) tabs1)
       -> sql_from_items free (map (map (fn p :: (Type * Type) => p.1)) tabs1) -> sql_from_items free tabs2
       -> sql_exp (free ++ map (map (fn p :: (Type * Type) => p.1)) tabs1 ++ tabs2) [] [] bool
       -> sql_from_items free (map (map (fn p :: (Type * Type) => p.2)) tabs1 ++ tabs2)

val sql_full_join : free ::: {{Type}} -> tabs1 ::: {{(Type * Type)}} -> tabs2 ::: {{(Type * Type)}}
                     -> [free ~ tabs1] => [free ~ tabs2] => [tabs1 ~ tabs2]
    => $(map (fn r => $(map (fn p :: (Type * Type) => nullify p.1 p.2) r)) (tabs1 ++ tabs2))
       -> sql_from_items free (map (map (fn p :: (Type * Type) => p.1)) tabs1)
       -> sql_from_items free (map (map (fn p :: (Type * Type) => p.1)) tabs2)
       -> sql_exp (free ++ map (map (fn p :: (Type * Type) => p.1)) (tabs1 ++ tabs2)) [] [] bool
       -> sql_from_items free (map (map (fn p :: (Type * Type) => p.2)) (tabs1 ++ tabs2))

val sql_query1 : free ::: {{Type}}
                 -> afree ::: {{Type}}
                 -> tables ::: {{Type}}
                 -> grouped ::: {{Type}}
                 -> selectedFields ::: {{Type}}
                 -> selectedExps ::: {Type}
                 -> empties :: {Unit}
                 -> [free ~ tables]
                 => [free ~ grouped]
                 => [afree ~ tables]
                 => [empties ~ selectedFields]
                 => {Distinct : bool,
                     From : sql_from_items free tables,
                     Where : sql_exp (free ++ tables) afree [] bool,
                     GroupBy : sql_subset tables grouped,
                     Having : sql_exp (free ++ grouped) (afree ++ tables) [] bool,
                     SelectFields : sql_subset grouped (map (fn _ => []) empties ++ selectedFields),
                     SelectExps : $(map (sql_exp (free ++ grouped) (afree ++ tables) [])
                                            selectedExps) }
                 -> sql_query1 free afree tables selectedFields selectedExps

type sql_relop 
val sql_union : sql_relop
val sql_intersect : sql_relop
val sql_except : sql_relop
val sql_relop : free ::: {{Type}}
                -> afree ::: {{Type}}
                -> tables1 ::: {{Type}}
                -> tables2 ::: {{Type}}
                -> selectedFields ::: {{Type}}
                -> selectedExps ::: {Type}
                -> sql_relop
                -> bool (* ALL *)
                -> sql_query1 free afree tables1 selectedFields selectedExps
                -> sql_query1 free afree tables2 selectedFields selectedExps
                -> sql_query1 free afree [] selectedFields selectedExps
val sql_forget_tables : free ::: {{Type}} -> afree ::: {{Type}} -> tables ::: {{Type}} -> selectedFields ::: {{Type}} -> selectedExps ::: {Type}
                        -> sql_query1 free afree tables selectedFields selectedExps
                        -> sql_query1 free afree [] selectedFields selectedExps

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

val sql_query : free ::: {{Type}}
                -> afree ::: {{Type}}
                -> tables ::: {{Type}}
                -> selectedFields ::: {{Type}}
                -> selectedExps ::: {Type}
                -> [free ~ tables]
                => {Rows : sql_query1 free afree tables selectedFields selectedExps,
                    OrderBy : sql_order_by (free ++ tables) selectedExps,
                    Limit : sql_limit,
                    Offset : sql_offset}
                -> sql_query free afree selectedFields selectedExps

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
val sql_arith_int : sql_arith int
val sql_arith_float : sql_arith float
val sql_arith_option : t ::: Type -> sql_arith t -> sql_arith (option t)

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

con sql_aggregate :: Type -> Type -> Type
val sql_aggregate : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                    -> dom ::: Type -> ran ::: Type
                    -> sql_aggregate dom ran -> sql_exp agg agg exps dom
                    -> sql_exp tables agg exps ran

val sql_count_col : t ::: Type -> sql_aggregate (option t) int

class sql_summable
val sql_summable_int : sql_summable int
val sql_summable_float : sql_summable float
val sql_summable_option : t ::: Type -> sql_summable t -> sql_summable (option t)
val sql_avg : t ::: Type -> nt ::: Type -> sql_summable t -> nullify t nt -> sql_aggregate t nt
val sql_sum : t ::: Type -> nt ::: Type -> sql_summable t -> nullify t nt -> sql_aggregate t nt

class sql_maxable
val sql_maxable_int : sql_maxable int
val sql_maxable_float : sql_maxable float
val sql_maxable_string : sql_maxable string
val sql_maxable_time : sql_maxable time
val sql_maxable_option : t ::: Type -> sql_maxable t -> sql_maxable (option t)
val sql_max : t ::: Type -> nt ::: Type -> sql_maxable t -> nullify t nt -> sql_aggregate t nt
val sql_min : t ::: Type -> nt ::: Type -> sql_maxable t -> nullify t nt -> sql_aggregate t nt

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
val sql_known : t ::: Type -> sql_ufunc t bool


val sql_nullable : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} -> t ::: Type
                   -> sql_injectable_prim t
                   -> sql_exp tables agg exps t
                   -> sql_exp tables agg exps (option t)

val sql_subquery : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} -> nm ::: Name -> t ::: Type -> nt ::: Type
                   -> nullify t nt
                   -> sql_query tables agg [] [nm = t]
                   -> sql_exp tables agg exps nt

(*** Executing queries *)

val query : tables ::: {{Type}} -> exps ::: {Type}
            -> [tables ~ exps] =>
                  state ::: Type
                  -> sql_query [] [] tables exps
                  -> ($(exps ++ map (fn fields :: {Type} => $fields) tables)
                      -> state
                      -> transaction state)
                  -> state
                  -> transaction state


(*** Database mutators *)

type dml
val dml : dml -> transaction unit
val tryDml : dml -> transaction (option string)
(* Returns an error message on failure. *)

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
val setval : sql_sequence -> int -> transaction unit


(** XML *)

type css_class
val show_css_class : show css_class
val classes : css_class -> css_class -> css_class
(* The equivalent of writing one class after the other, separated by a space, in
 * an HTML 'class' attribute *)

con tag :: {Type} -> {Unit} -> {Unit} -> {Type} -> {Type} -> Type

con xml :: {Unit} -> {Type} -> {Type} -> Type
val cdata : ctx ::: {Unit} -> use ::: {Type} -> string -> xml ctx use []
val cdataChar : ctx ::: {Unit} -> use ::: {Type} -> char -> xml ctx use []
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
con xtable = xml [Body, Table] [] []
con xtr = xml [Body, Tr] [] []
con xform = xml [Body, Form] [] []


(*** HTML details *)

con html = [Html]
con head = [Head]
con body = [Body]
con form = [Body, Form]
con subform = [Body, Subform]
con tabl = [Body, Table]
con tr = [Body, Tr]

type queryString
val show_queryString : show queryString

type url
val show_url : show url
val bless : string -> url
val checkUrl : string -> option url
val currentUrl : transaction url
val currentUrlHasPost : transaction bool
val currentUrlHasQueryString : transaction bool
val url : transaction page -> url
val effectfulUrl : (option queryString -> transaction page) -> url
val redirect : t ::: Type -> url -> transaction t

type id
val fresh : transaction id

val dyn : ctx ::: {Unit} -> use ::: {Type} -> bind ::: {Type} -> [ctx ~ body] => unit
          -> tag [Signal = signal (xml (body ++ ctx) use bind)] (body ++ ctx) [] use bind

val head : unit -> tag [] html head [] []
val title : unit -> tag [] head [] [] []
val link : unit -> tag [Id = id, Rel = string, Typ = string, Href = url, Media = string] head [] [] []

val body : unit -> tag [Onload = transaction unit, Onresize = transaction unit, Onunload = transaction unit]
                       html body [] []
con bodyTag = fn (attrs :: {Type}) =>
                 ctx ::: {Unit} ->
                 [[Body] ~ ctx] =>
                    unit -> tag attrs ([Body] ++ ctx) ([Body] ++ ctx) [] []
con bodyTagStandalone = fn (attrs :: {Type}) =>
                           ctx ::: {Unit}
                           -> [[Body] ~ ctx] =>
                                 unit -> tag attrs ([Body] ++ ctx) [] [] []

val br : bodyTagStandalone [Id = id]

con focusEvents = [Onblur = transaction unit, Onfocus = transaction unit]
con mouseEvents = [Onclick = transaction unit, Ondblclick = transaction unit,
                   Onmousedown = transaction unit, Onmousemove = transaction unit,
                   Onmouseout = transaction unit, Onmouseover = transaction unit,
                   Onmouseup = transaction unit]
con keyEvents = [Onkeydown = int -> transaction unit, Onkeypress = int -> transaction unit,
                 Onkeyup = int -> transaction unit]
(* Key arguments are character codes. *)
con resizeEvents = [Onresize = transaction unit]

con boxEvents = focusEvents ++ mouseEvents ++ keyEvents ++ resizeEvents
con tableEvents = focusEvents ++ mouseEvents ++ keyEvents

con boxAttrs = [Id = id, Title = string] ++ boxEvents
con tableAttrs = [Id = id, Title = string] ++ tableEvents

val span : bodyTag boxAttrs
val div : bodyTag boxAttrs

val p : bodyTag boxAttrs
val b : bodyTag boxAttrs
val i : bodyTag boxAttrs
val tt : bodyTag boxAttrs
val sub : bodyTag boxAttrs
val sup : bodyTag boxAttrs

val h1 : bodyTag boxAttrs
val h2 : bodyTag boxAttrs
val h3 : bodyTag boxAttrs
val h4 : bodyTag boxAttrs
val h5 : bodyTag boxAttrs
val h6 : bodyTag boxAttrs

val li : bodyTag boxAttrs
val ol : bodyTag boxAttrs
val ul : bodyTag boxAttrs

val hr : bodyTag boxAttrs

val a : bodyTag ([Link = transaction page, Href = url, Target = string] ++ boxAttrs)

val img : bodyTag ([Alt = string, Src = url, Width = int, Height = int,
                    Onabort = transaction unit, Onerror = transaction unit,
                    Onload = transaction unit] ++ boxAttrs)
          
val form : ctx ::: {Unit} -> bind ::: {Type}
           -> [[Body, Form, Table] ~ ctx] =>
    option css_class
    -> xml ([Body, Form] ++ ctx) [] bind
    -> xml ([Body] ++ ctx) [] []
       
val subform : ctx ::: {Unit} -> use ::: {Type} -> bind ::: {Type}
              -> [[Form] ~ ctx] =>
    nm :: Name -> [[nm] ~ use] =>
    xml ([Form] ++ ctx) [] bind
    -> xml ([Form] ++ ctx) use [nm = $bind]

val subforms : ctx ::: {Unit} -> use ::: {Type} -> bind ::: {Type}
              -> [[Form, Subform] ~ ctx] =>
    nm :: Name -> [[nm] ~ use] =>
    xml ([Subform] ++ ctx) [Entry = $bind] []
    -> xml ([Form] ++ ctx) use [nm = list ($bind)]

val entry : ctx ::: {Unit} -> bind ::: {Type}
              -> [[Subform, Form] ~ ctx] =>
    xml ([Form] ++ ctx) [] bind
    -> xml ([Subform] ++ ctx) [Entry = $bind] []

con formTag = fn (ty :: Type) (inner :: {Unit}) (attrs :: {Type}) =>
                  ctx ::: {Unit}
                  -> [[Form] ~ ctx] =>
                        nm :: Name -> unit
                        -> tag attrs ([Form] ++ ctx) inner [] [nm = ty]
val hidden : formTag string [] [Id = string, Value = string]
val textbox : formTag string [] ([Value = string, Size = int, Source = source string, Onchange = transaction unit,
                                  Ontext = transaction unit] ++ boxAttrs)
val password : formTag string [] ([Value = string, Size = int] ++ boxAttrs)
val textarea : formTag string [] ([Rows = int, Cols = int, Onchange = transaction unit,
                                   Ontext = transaction unit] ++ boxAttrs)

val checkbox : formTag bool [] ([Checked = bool] ++ boxAttrs)

type file
val fileName : file -> option string
val fileMimeType : file -> string
val fileData : file -> blob

val upload : formTag file [] ([Value = string, Size = int] ++ boxAttrs)

type mimeType
val blessMime : string -> mimeType
val checkMime : string -> option mimeType
val returnBlob : t ::: Type -> blob -> mimeType -> transaction t
val blobSize : blob -> int
val textBlob : string -> blob

type postBody
val postType : postBody -> string
val postData : postBody -> string

con radio = [Body, Radio]
val radio : formTag string radio [Id = id]
val radioOption : unit -> tag ([Value = string, Checked = bool] ++ boxAttrs) radio [] [] []

con select = [Select]
val select : formTag string select ([Onchange = transaction unit] ++ boxAttrs)
val option : unit -> tag [Value = string, Selected = bool] select [] [] []

val submit : ctx ::: {Unit} -> use ::: {Type}
             -> [[Form] ~ ctx] =>
                   unit
                   -> tag ([Value = string, Action = $use -> transaction page] ++ boxAttrs)
                          ([Form] ++ ctx) ([Form] ++ ctx) use []

val image : ctx ::: {Unit} -> use ::: {Type}
             -> [[Form] ~ ctx] =>
                   unit
                   -> tag ([Src = url, Width = int, Height = int, Alt = string, Action = $use -> transaction page] ++ boxAttrs)
                          ([Form] ++ ctx) ([Form] ++ ctx) use []

val label : bodyTag ([For = id, Accesskey = string] ++ tableAttrs)


(*** AJAX-oriented widgets *)

con cformTag = fn (attrs :: {Type}) (inner :: {Unit}) =>
                  ctx ::: {Unit}
                  -> [[Body] ~ ctx] =>
                        unit -> tag attrs ([Body] ++ ctx) inner [] []

val ctextbox : cformTag ([Value = string, Size = int, Source = source string, Onchange = transaction unit,
                          Ontext = transaction unit] ++ boxAttrs) []
val button : cformTag ([Value = string] ++ boxAttrs) []

val ccheckbox : cformTag ([Value = bool, Size = int, Source = source bool] ++ boxAttrs) []

con cselect = [Cselect]
val cselect : cformTag ([Source = source string, Onchange = transaction unit] ++ boxAttrs) cselect
val coption : unit -> tag [Value = string, Selected = bool] cselect [] [] []

val ctextarea : cformTag ([Value = string, Rows = int, Cols = int, Source = source string, Onchange = transaction unit,
                           Ontext = transaction unit] ++ boxAttrs) []

(*** Tables *)

val tabl : other ::: {Unit} -> [other ~ [Body, Table]] => unit
  -> tag ([Border = int] ++ boxAttrs)
         ([Body] ++ other) ([Body, Table] ++ other) [] []
val tr : other ::: {Unit} -> [other ~ [Body, Table, Tr]] => unit
  -> tag tableAttrs
         ([Body, Table] ++ other) ([Body, Tr] ++ other) [] []
val th : other ::: {Unit} -> [other ~ [Body, Tr]] => unit
  -> tag ([Colspan = int, Rowspan = int] ++ tableAttrs)
         ([Body, Tr] ++ other) ([Body] ++ other) [] []
val td : other ::: {Unit} -> [other ~ [Body, Tr]] => unit
  -> tag ([Colspan = int, Rowspan = int] ++ tableAttrs)
         ([Body, Tr] ++ other) ([Body] ++ other) [] []


(** Aborting *)

val error : t ::: Type -> xbody -> t

(* Client-side-only handlers: *)
val onError : (xbody -> transaction unit) -> transaction unit
val onFail : (string -> transaction unit) -> transaction unit
val onConnectFail : transaction unit -> transaction unit
val onDisconnect : transaction unit -> transaction unit
val onServerError : (string -> transaction unit) -> transaction unit

(* More standard document-level JavaScript handlers *)
val onClick : transaction unit -> transaction unit
val onDblclick : transaction unit -> transaction unit
val onKeydown : (int -> transaction unit) -> transaction unit
val onKeypress : (int -> transaction unit) -> transaction unit
val onKeyup : (int -> transaction unit) -> transaction unit
val onMousedown : transaction unit -> transaction unit
val onMouseup : transaction unit -> transaction unit

(* Prevents default handling of current event *)
val preventDefault : transaction unit
(* Stops propagation of current event *)
val stopPropagation : transaction unit

val show_xml : ctx ::: {Unit} -> use ::: {Type} -> bind ::: {Type} -> show (xml ctx use bind)


(** Tasks *)

con task_kind :: Type -> Type
val initialize : task_kind unit
val clientLeaves : task_kind client
val periodic : int -> task_kind unit


(** Information flow security *)

type sql_policy

val sendClient : tables ::: {{Type}} -> exps ::: {Type}
                 -> [tables ~ exps] => sql_query [] [] tables exps
                 -> sql_policy

val sendOwnIds : sql_sequence -> sql_policy

val mayInsert : fs ::: {Type} -> tables ::: {{Type}} -> [[New] ~ tables]
                => sql_query [] [] ([New = fs] ++ tables) []
                -> sql_policy

val mayDelete : fs ::: {Type} -> tables ::: {{Type}} -> [[Old] ~ tables]
                => sql_query [] [] ([Old = fs] ++ tables) []
                -> sql_policy

val mayUpdate : fs ::: {Type} -> tables ::: {{Type}} -> [[Old, New] ~ tables]
                => sql_query [] [] ([Old = fs, New = fs] ++ tables) []
                -> sql_policy

val also : sql_policy -> sql_policy -> sql_policy

val debug : string -> transaction unit
val naughtyDebug : string -> int

val rand : transaction int
