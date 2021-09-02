type int
type float
type string
type char
type time
type blob
type calendardate
type clocktime

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
val pow : t ::: Type -> num t -> t -> t -> t
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

val iscodepoint : int -> bool
val issingle : char -> bool

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

(* These last three reveal the Unicode encoding,
 * into sequences of 8-bit bytes. *)
val strlenUtf8 : string -> int
val strsubUtf8 : string -> int -> char
val strsuffixUtf8 : string -> int -> string

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


(** * Floats *)

val float : int -> float
val ceil : float -> int
val trunc : float -> int
val round : float -> int
val floor : float -> int

(** * Basic Math *)

val sqrt : float -> float
val sin : float -> float
val cos : float -> float
val log : float -> float
val exp : float -> float
val asin : float -> float
val acos : float -> float
val atan : float -> float
val atan2 : float -> float -> float
val abs: float -> float

(** * Time *)

val now : transaction time
val minTime : time
val addSeconds : time -> int -> time
val toSeconds : time -> int
val diffInSeconds : time -> time -> int
(* Earlier time first *)
val toMilliseconds : time -> int
val fromMilliseconds : int -> time
val diffInMilliseconds : time -> time -> int
val timef : string -> time -> string (* Uses strftime() format string *)
val readUtc : string -> option time

(* Takes a year, month, day, hour, minute, second. *)
val fromDatetime : int -> int -> int -> int -> int -> int -> time
val datetimeYear : time -> int
val datetimeMonth : time -> int
val datetimeDay : time -> int
val datetimeHour : time -> int
val datetimeMinute: time -> int
val datetimeSecond : time -> int
val datetimeDayOfWeek : time -> int

(** * Calendardate *)
val getCurrentCalendardate: transaction calendardate
val getYear: calendardate -> int
val getMonth: calendardate -> int
val getDay: calendardate -> int

(** * Clocktime *)
val getCurrentClocktime: transaction clocktime
val getHour: clocktime -> int
val getMinute: clocktime -> int

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

type envVar
val blessEnvVar : string -> envVar
val checkEnvVar : string -> option envVar
val getenv : envVar -> transaction (option string)

type meta
val blessMeta : string -> meta
val checkMeta : string -> option meta


(** JavaScript-y gadgets *)

val alert : string -> transaction unit
val confirm : string -> transaction bool
val spawn : transaction unit -> transaction unit
val sleep : int -> transaction unit

val rpc : t ::: Type -> transaction t -> transaction t
val tryRpc : t ::: Type -> transaction t -> transaction (option t)
(* Returns [None] on error condition. *)


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
val sql_calendardate : sql_injectable_prim calendardate
val sql_clocktime : sql_injectable_prim clocktime
val sql_blob : sql_injectable_prim blob
val sql_channel : t ::: Type -> sql_injectable_prim (channel t)
val sql_client : sql_injectable_prim client

con serialized :: Type -> Type
val serialize : t ::: Type -> t -> serialized t
val deserialize : t ::: Type -> serialized t -> t
val sql_serialized : t ::: Type -> sql_injectable_prim (serialized t)
val unsafeSerializedToString : t ::: Type -> serialized t -> string
val unsafeSerializedFromString : t ::: Type -> string -> serialized t

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

(*** Indices *)

con index_mode :: Type -> Type
val equality : t ::: Type -> index_mode t
val trigram : index_mode string (* only in Postgres, for now *)
val skipped : t ::: Type -> index_mode t (* handy for building these descriptions programmatically,
                                          * when not all columns in a row should be indexed *)

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

(** [ORDER BY] and [SELECT] expressions may use window functions, so we introduce a type family for such expressions. *)
con sql_expw :: {{Type}} -> {{Type}} -> {Type} -> Type -> Type

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
                     SelectExps : $(map (sql_expw (free ++ grouped) (afree ++ tables) [])
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

(** This type class supports automatic injection of either regular or window expressions into [sql_expw]. *)
class sql_window :: ({{Type}} -> {{Type}} -> {Type} -> Type -> Type) -> Type
val sql_window_normal : sql_window sql_exp
val sql_window_fancy : sql_window sql_expw
val sql_window : tf ::: ({{Type}} -> {{Type}} -> {Type} -> Type -> Type)
                 -> tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} -> t ::: Type
                 -> sql_window tf
                 -> tf tables agg exps t
                 -> sql_expw tables agg exps t

con sql_order_by :: {{Type}} -> {Type} -> Type
val sql_order_by_Nil : tables ::: {{Type}} -> exps :: {Type} -> sql_order_by tables exps
val sql_order_by_Cons : tf ::: ({{Type}} -> {{Type}} -> {Type} -> Type -> Type) -> tables ::: {{Type}} -> exps ::: {Type} -> t ::: Type
                        -> sql_window tf
                        -> tf tables [] exps t -> sql_direction
                        -> sql_order_by tables exps
                        -> sql_order_by tables exps
val sql_order_by_random : tables ::: {{Type}} -> exps ::: {Type}
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

val sql_coalesce : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                  -> t ::: Type
                  -> sql_exp tables agg exps (option t)
                  -> sql_exp tables agg exps t
                  -> sql_exp tables agg exps t

val sql_if_then_else : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                       -> t ::: Type
                       -> sql_exp tables agg exps bool
                       -> sql_exp tables agg exps t
                       -> sql_exp tables agg exps t
                       -> sql_exp tables agg exps t

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

val sql_like : sql_binary string string bool
val sql_distance : sql_binary string string float

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
val sql_avg : t ::: Type -> sql_summable t -> sql_aggregate t (option float)
val sql_sum : t ::: Type -> nt ::: Type -> sql_summable t -> nullify t nt -> sql_aggregate t nt

class sql_maxable
val sql_maxable_int : sql_maxable int
val sql_maxable_float : sql_maxable float
val sql_maxable_string : sql_maxable string
val sql_maxable_time : sql_maxable time
val sql_maxable_clocktime : sql_maxable clocktime
val sql_maxable_calendardate : sql_maxable calendardate
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
val sql_lower : sql_ufunc string string
val sql_upper : sql_ufunc string string

con sql_bfunc :: Type -> Type -> Type -> Type
val sql_bfunc : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                -> dom1 ::: Type -> dom2 ::: Type -> ran ::: Type
                -> sql_bfunc dom1 dom2 ran
                -> sql_exp tables agg exps dom1
                -> sql_exp tables agg exps dom2
                -> sql_exp tables agg exps ran
val sql_similarity : sql_bfunc string string float
(* Only supported by Postgres for now, via the pg_trgm module *)

val sql_nullable : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} -> t ::: Type
                   -> sql_injectable_prim t
                   -> sql_exp tables agg exps t
                   -> sql_exp tables agg exps (option t)

val sql_subquery : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} -> nm ::: Name -> t ::: Type -> nt ::: Type
                   -> nullify t nt
                   -> sql_query tables agg [] [nm = t]
                   -> sql_exp tables agg exps nt

(** Window function expressions *)

con sql_partition :: {{Type}} -> {{Type}} -> {Type} -> Type
val sql_no_partition : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                       -> sql_partition tables agg exps
val sql_partition : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} -> t ::: Type
                    -> sql_exp tables agg exps t
                    -> sql_partition tables agg exps

con sql_window_function :: {{Type}} -> {{Type}} -> {Type} -> Type -> Type
val sql_window_function : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                          -> t ::: Type
                          -> sql_window_function tables agg exps t
                          -> sql_partition tables agg exps
                          -> sql_order_by tables exps
                          -> sql_expw tables agg exps t

val sql_window_aggregate : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                           -> t ::: Type -> nt ::: Type
                           -> sql_aggregate t nt
                           -> sql_exp tables agg exps t
                           -> sql_window_function tables agg exps nt
val sql_window_count : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                       -> sql_window_function tables agg exps int
val sql_rank : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
               -> sql_window_function tables agg exps int


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

val show_sql_query : freeTables ::: {{Type}} -> freeAggs ::: {{Type}} -> tables ::: {{Type}} -> exps ::: {Type}
                     -> show (sql_query freeTables freeAggs tables exps)


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
val null : css_class
(* No special formatting *)
val classes : css_class -> css_class -> css_class
(* The equivalent of writing one class after the other, separated by a space, in
 * an HTML 'class' attribute *)

type css_value
val atom : string -> css_value
type url
val css_url : url -> css_value
val sql_url : sql_injectable_prim url
type css_property
val property : string -> css_property
val value : css_property -> css_value -> css_property
type css_style
val noStyle : css_style
val oneProperty : css_style -> css_property -> css_style

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
           css_class
	   -> option (signal css_class)
           -> css_style
	   -> option (signal css_style)
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

con html = [Html]
con head = [Head]

con body' = [MakeForm, Body]
con form' = [Body, Form]
con subform' = [Body, Subform]
con tabl' = [MakeForm, Table]
con tr' = [MakeForm, Tr]

con body = [Dyn] ++ body'
con form = [Dyn] ++ form'
con subform = [Dyn] ++ subform'
con tabl = [Dyn] ++ tabl'
con tr = [Dyn] ++ tr'

con xhtml = xml html
con page = xhtml [] []
con xbody = xml body [] []
con xhead = xml head [] []
con xtable = xml tabl [] []
con xtr = xml tr [] []
con xform = xml form [] []


(*** HTML details *)

type queryString
val show_queryString : show queryString

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
val giveFocus : id -> transaction unit
val show_id : show id
val anchorUrl : id -> url

val dyn : ctx ::: {Unit} -> use ::: {Type} -> bind ::: {Type} -> [ctx ~ [Dyn]] => unit
          -> tag [Signal = signal (xml ([Dyn] ++ ctx) use bind)] ([Dyn] ++ ctx) [] use bind

val active : unit
             -> tag [Code = transaction xbody] body [] [] []

val script : unit
             -> tag [Code = transaction unit] head [] [] []

(* Type for HTML5 "data-*" and "aria-*" attributes. *)
type data_attr_kind
val data_kind : data_attr_kind
val aria_kind : data_attr_kind

type data_attr
val data_attr : data_attr_kind -> string (* Key *) -> string (* Value *) -> data_attr
(* This function will fail if the key doesn't meet HTML's lexical rules! *)
val data_attrs : data_attr -> data_attr -> data_attr

val head : unit -> tag [Data = data_attr] html head [] []
val title : unit -> tag [Data = data_attr] head [] [] []
val link : unit -> tag [Data = data_attr, Id = id, Rel = string, Title = string, Typ = string, Href = url, Media = string, Integrity = string, Crossorigin = string, Sizes = string] head [] [] []
val meta : unit -> tag [Nam = meta, Content = string, Id = id] head [] [] []

datatype mouseButton = Left | Right | Middle

type mouseEvent = { ScreenX : int, ScreenY : int, ClientX : int, ClientY : int, OffsetX : int, OffsetY : int,
                    CtrlKey : bool, ShiftKey : bool, AltKey : bool, MetaKey : bool,
                    Button : mouseButton }

con mouseEvents = map (fn _ :: Unit => mouseEvent -> transaction unit)
                          [Onclick, Oncontextmenu, Ondblclick, Onmousedown, Onmouseenter, Onmouseleave, Onmousemove, Onmouseout, Onmouseover, Onmouseup]

(* Key arguments are character codes. *)
type keyEvent = { KeyCode : int,
                  CtrlKey : bool, ShiftKey : bool, AltKey : bool, MetaKey : bool }

con keyEvents = map (fn _ :: Unit => keyEvent -> transaction unit)
                        [Onkeydown, Onkeypress, Onkeyup]

con focusEvents = [Onblur = transaction unit, Onfocus = transaction unit]

con resizeEvents = [Onresize = transaction unit]
con scrollEvents = [Onscroll = transaction unit]

con boxEvents = focusEvents ++ mouseEvents ++ keyEvents ++ resizeEvents ++ scrollEvents
con tableEvents = focusEvents ++ mouseEvents ++ keyEvents

con boxAttrs = [Data = data_attr, Id = id, Title = string, Role = string, Align = string] ++ boxEvents
con tableAttrs = [Data = data_attr, Id = id, Title = string, Align = string] ++ tableEvents

val body : unit -> tag ([Data = data_attr, Id = id, Title = string, Onload = transaction unit, Onunload = transaction unit, Onhashchange = transaction unit]
                            ++ boxEvents)
                       html body [] []

con bodyTag = fn (attrs :: {Type}) =>
                 ctx ::: {Unit} ->
                 [[Body] ~ ctx] =>
                    unit -> tag attrs ([Body] ++ ctx) ([Body] ++ ctx) [] []
con bodyTagStandalone = fn (attrs :: {Type}) =>
                           ctx ::: {Unit}
                           -> [[Body] ~ ctx] =>
                                 unit -> tag attrs ([Body] ++ ctx) [] [] []

val br : bodyTagStandalone [Data = data_attr, Id = id]

val span : bodyTag boxAttrs
val div : bodyTag boxAttrs

val p : bodyTag boxAttrs
val strong : bodyTag boxAttrs
val em : bodyTag boxAttrs
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

val pre : bodyTag boxAttrs

(** sections **)
val section : bodyTag boxAttrs
val article : bodyTag boxAttrs
val nav : bodyTag boxAttrs
val aside : bodyTag boxAttrs
val footer : bodyTag boxAttrs
val header : bodyTag boxAttrs
val main : bodyTag boxAttrs

(** forms **)
val meter : bodyTag boxAttrs
val progress : bodyTag boxAttrs
val output : bodyTag boxAttrs
val keygen : bodyTag boxAttrs
val datalist : bodyTag boxAttrs

(** Interactive Elements **)
val details : bodyTag boxAttrs
val dialog : bodyTag boxAttrs
val menuitem : bodyTag boxAttrs

(** Grouping Content **)
val figure : bodyTag boxAttrs
val figcaption : bodyTag boxAttrs

(** Text Level Semantics **)
val data : bodyTag boxAttrs
val mark : bodyTag boxAttrs
val rp  : bodyTag boxAttrs
val rt  : bodyTag boxAttrs
val ruby : bodyTag boxAttrs
val summary : bodyTag boxAttrs
val time  : bodyTag boxAttrs
val wbr : bodyTag boxAttrs
val bdi : bodyTag boxAttrs

val a : bodyTag ([Link = transaction page, Href = url, Target = string, Rel = string, Download = string] ++ boxAttrs)

val img : bodyTag ([Alt = string, Src = url, Width = int, Height = int,
                    Onabort = transaction unit, Onerror = transaction unit,
                    Onload = transaction unit] ++ boxAttrs)

val form : ctx ::: {Unit} -> bind ::: {Type}
           -> [[MakeForm, Form] ~ ctx] =>
    option id
    -> option css_class
    -> xml ([Form] ++ ctx) [] bind
    -> xml ([MakeForm] ++ ctx) [] []

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

con inputAttrs' = [Required = bool, Autofocus = bool,
                   Onchange = transaction unit]
con inputAttrs = inputAttrs' ++ [Oninput = transaction unit]

val hidden : formTag string [] [Data = data_attr, Id = string, Value = string]
val textbox : formTag string [] ([Value = string, Size = int, Placeholder = string, Source = source string] ++ boxAttrs ++ inputAttrs)
val password : formTag string [] ([Value = string, Size = int, Placeholder = string] ++ boxAttrs ++ inputAttrs)
val textarea : formTag string [] ([Rows = int, Cols = int, Placeholder = string] ++ boxAttrs ++ inputAttrs)

val checkbox : formTag bool [] ([Checked = bool] ++ boxAttrs ++ inputAttrs')

(* HTML5 widgets galore! *)

type textWidget = formTag string [] ([Value = string, Size = int, Placeholder = string] ++ boxAttrs ++ inputAttrs)

val email : textWidget
val search : textWidget
val url_ : textWidget
val tel : textWidget
val color : textWidget

val number : formTag float [] ([Value = float, Min = float, Max = float, Step = float, Size = int] ++ boxAttrs ++ inputAttrs)
val range : formTag float [] ([Value = float, Min = float, Max = float, Size = int] ++ boxAttrs ++ inputAttrs)
val date : formTag string [] ([Value = string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs)
val datetime : formTag string [] ([Value = string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs)
val datetime_local : formTag string [] ([Value = string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs)
val month : formTag string [] ([Value = string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs)
val week : formTag string [] ([Value = string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs)
val timeInput : formTag string [] ([Value = string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs)



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
val textOfBlob : blob -> option string
(* Returns [Some] exactly when the blob contains no zero bytes. *)

type postBody
val postType : postBody -> string
val postData : postBody -> string

type postField
val firstFormField : string -> option postField
val fieldName : postField -> string
val fieldValue : postField -> string
val remainingFields : postField -> string

con radio = [Body, Radio]
val radio : formTag (option string) radio [Data = data_attr, Id = id]
val radioOption : unit -> tag ([Value = string, Checked = bool] ++ boxAttrs ++ inputAttrs') radio [] [] []

con select = [Select]
val select : formTag string select (boxAttrs ++ inputAttrs' ++ [Multiple = bool])
val option : unit -> tag [Data = data_attr, Value = string, Selected = bool] select [] [] []

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

val fieldset : bodyTag boxAttrs
val legend : bodyTag boxAttrs


(*** AJAX-oriented widgets *)

con cformTag = fn (attrs :: {Type}) (inner :: {Unit}) =>
                  ctx ::: {Unit}
                  -> [[Body] ~ ctx] => [[Body] ~ inner] =>
                        unit -> tag attrs ([Body] ++ ctx) ([Body] ++ inner) [] []

type ctext = cformTag ([Value = string, Size = int, Source = source string, Placeholder = string] ++ boxAttrs ++ inputAttrs) []

val ctextbox : ctext
val cpassword : ctext
val cemail : ctext
val csearch : ctext
val curl : ctext
val ctel : ctext
val ccolor : ctext

val cnumber : cformTag ([Source = source (option float), Min = float, Max = float, Step = float, Size = int] ++ boxAttrs ++ inputAttrs) []
val crange : cformTag ([Source = source (option float), Min = float, Max = float, Size = int, Step = float] ++ boxAttrs ++ inputAttrs) []
val cdate : cformTag ([Source = source string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs) []
val cdatetime : cformTag ([Source = source string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs) []
val cdatetime_local : cformTag ([Source = source string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs) []
val cmonth : cformTag ([Source = source string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs) []
val cweek : cformTag ([Source = source string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs) []
val ctime : cformTag ([Source = source string, Min = string, Max = string, Size = int] ++ boxAttrs ++ inputAttrs) []

val button : cformTag ([Value = string, Disabled = bool] ++ boxAttrs) []

val ccheckbox : cformTag ([Size = int, Source = source bool] ++ boxAttrs ++ inputAttrs') []

val cradio : cformTag ([Source = source (option string), Value = string] ++ boxAttrs ++ inputAttrs') []

val cselect : cformTag ([Source = source string, Multiple = bool] ++ boxAttrs ++ inputAttrs') [Cselect]
val coption : unit -> tag [Value = string, Selected = bool] [Cselect, Body] [] [] []

val ctextarea : cformTag ([Rows = int, Cols = int, Placeholder = string, Source = source string,
                           Ontext = transaction unit] ++ boxAttrs ++ inputAttrs) []

(*** Tables *)

val tabl : other ::: {Unit} -> [other ~ [Body, Table]] => unit
  -> tag ([Border = int] ++ boxAttrs)
         ([Body] ++ other) ([Table] ++ other) [] []
val tr : other ::: {Unit} -> [other ~ [Table, Tr]] => unit
  -> tag tableAttrs
         ([Table] ++ other) ([Tr] ++ other) [] []
val th : other ::: {Unit} -> [other ~ [Body, Tr]] => unit
  -> tag ([Colspan = int, Rowspan = int] ++ tableAttrs)
         ([Tr] ++ other) ([Body] ++ other) [] []
val td : other ::: {Unit} -> [other ~ [Body, Tr]] => unit
  -> tag ([Colspan = int, Rowspan = int] ++ tableAttrs)
         ([Tr] ++ other) ([Body] ++ other) [] []

val thead : other ::: {Unit} -> [other ~ [Table]] => unit
  -> tag tableAttrs
         ([Table] ++ other) ([Table] ++ other) [] []
val tbody : other ::: {Unit} -> [other ~ [Table]] => unit
  -> tag tableAttrs
         ([Table] ++ other) ([Table] ++ other) [] []
val tfoot : other ::: {Unit} -> [other ~ [Table]] => unit
  -> tag tableAttrs
         ([Table] ++ other) ([Table] ++ other) [] []

(** Definition lists *)

val dl : other ::: {Unit} -> [other ~ [Body,Dl]]
  => unit
  -> tag [Data = data_attr] ([Body] ++ other) ([Dl] ++ other) [] []

val dt : other ::: {Unit} -> [other ~ [Body,Dl]]
  => unit
  -> tag [Data = data_attr] ([Dl] ++ other) ([Body] ++ other) [] []

val dd : other ::: {Unit} -> [other ~ [Body,Dl]]
  => unit
  -> tag [Data = data_attr] ([Dl] ++ other) ([Body] ++ other) [] []


(** Aborting *)

val error : t ::: Type -> xbody -> t

(* Client-side-only handlers: *)
val onError : (xbody -> transaction unit) -> transaction unit
val onFail : (string -> transaction unit) -> transaction unit
val onConnectFail : transaction unit -> transaction unit
val onDisconnect : transaction unit -> transaction unit
val onServerError : (string -> transaction unit) -> transaction unit

(* More standard document-level JavaScript handlers *)
val onClick : (mouseEvent -> transaction unit) -> transaction unit
val onDblclick : (mouseEvent -> transaction unit) -> transaction unit
val onContextmenu : (mouseEvent -> transaction unit) -> transaction unit
val onKeydown : (keyEvent -> transaction unit) -> transaction unit
val onKeypress : (keyEvent -> transaction unit) -> transaction unit
val onKeyup : (keyEvent -> transaction unit) -> transaction unit
val onMousedown : (mouseEvent -> transaction unit) -> transaction unit
val onMouseenter : (mouseEvent -> transaction unit) -> transaction unit
val onMouseleave : (mouseEvent -> transaction unit) -> transaction unit
val onMousemove : (mouseEvent -> transaction unit) -> transaction unit
val onMouseout : (mouseEvent -> transaction unit) -> transaction unit
val onMouseover : (mouseEvent -> transaction unit) -> transaction unit
val onMouseup : (mouseEvent -> transaction unit) -> transaction unit

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
