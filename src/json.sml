(*******************************************************************************
*  Standard ML JSON parser
*  Copyright (C) 2010  Gian Perrone
*
*  This program is free software: you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program.  If not, see <http://www.gnu.org/licenses/>.
******************************************************************************)

signature JSON_CALLBACKS = 
sig
    type json_data

    val json_object   : json_data list -> json_data
    val json_pair     : string * json_data -> json_data
    val json_array    : json_data list -> json_data
    val json_value    : json_data -> json_data
    val json_string   : string -> json_data
    val json_int      : int -> json_data
    val json_real     : real -> json_data
    val json_bool     : bool -> json_data
    val json_null     : unit -> json_data

    val error_handle  : string * int * string -> json_data
end

functor JSONParser (Callbacks : JSON_CALLBACKS) =
struct
   type json_data = Callbacks.json_data

   exception JSONParseError of string * int

   val inputData = ref ""
   val inputPosition = ref 0

   fun isDigit () = Char.isDigit (String.sub (!inputData,0))

   fun ws () = while (String.isPrefix " " (!inputData) orelse
                      String.isPrefix "\n" (!inputData) orelse
                      String.isPrefix "\t" (!inputData) orelse
                      String.isPrefix "\r" (!inputData))
               do (inputData := String.extract (!inputData, 1, NONE))

   fun peek () = String.sub (!inputData,0)
   fun take () = 
      String.sub (!inputData,0) before 
         inputData := String.extract (!inputData, 1, NONE)

   fun matches s = (ws(); String.isPrefix s (!inputData))
   fun consume s =
      if matches s then 
         (inputData := String.extract (!inputData, size s, NONE);
          inputPosition := !inputPosition + size s)
                   else 
         raise JSONParseError ("Expected '"^s^"'", !inputPosition)

   fun parseObject () =
      if not (matches "{") then 
         raise JSONParseError ("Expected '{'", !inputPosition)
      else 
         (consume "{"; ws ();
          if matches "}" then Callbacks.json_object [] before consume "}"
          else 
            (Callbacks.json_object (parseMembers ()) 
               before (ws (); consume "}")))

   and parseMembers () =
      parsePair () :: 
         (if matches "," then (consume ","; parseMembers ()) else [])

   and parsePair () =
      Callbacks.json_pair (parseString (),
                           (ws(); consume ":"; ws(); parseValue ()))

   and parseArray () =
      if not (matches "[") then 
         raise JSONParseError ("Expected '['", !inputPosition)
      else 
        (consume "[";
         if matches "]" then
            Callbacks.json_array [] before consume "]" 
         else
            Callbacks.json_array (parseElements ()) before (ws (); consume "]"))

   and parseElements () =
      parseValue () ::
         (if matches "," then (consume ","; parseElements ()) else [])

   and parseValue () =
      Callbacks.json_value (
         if matches "\"" then Callbacks.json_string (parseString ()) else
         if matches "-" orelse isDigit () then parseNumber () else
         if matches "true" then Callbacks.json_bool true before consume "true" else
         if matches "false" then Callbacks.json_bool false before consume "false" else
         if matches "null" then Callbacks.json_null () before consume "null" else
         if matches "[" then parseArray () else
         if matches "{" then parseObject () else
         raise JSONParseError ("Expected value", !inputPosition))

   and parseString () =
        (ws () ;
         consume ("\"") ;
         parseChars () before consume "\"")

   and parseChars () = 
   let
       val escapedchars = ["n", "r", "b", "f", "t"]
      fun pickChars s =
          if peek () = #"\"" (* " = end of string *)
          then s
          else
              if peek () = #"\\" andalso (String.sub (!inputData, 1)) = #"\""
              then (consume "\\\""; pickChars (s ^ "\""))
              else
                  if peek () = #"\\" andalso String.sub (!inputData, 1) = #"\\" andalso String.sub (!inputData, 2) = #"n"
                  then (consume "\\\\n"; pickChars (s ^ "\\n"))
                  else
                      if peek () = #"\\" andalso (String.sub (!inputData, 1)) = #"n"
                      then (consume "\\n"; pickChars (s ^ "\n"))
                      else
                          if peek () = #"\\" andalso String.sub (!inputData, 1) = #"\\" andalso String.sub (!inputData, 2) = #"r"
                          then (consume "\\\\r"; pickChars (s ^ "\\r"))
                          else
                              if peek () = #"\\" andalso (String.sub (!inputData, 1)) = #"r"
                              then (consume "\\r"; pickChars (s ^ "\r"))
                              else pickChars (s ^ String.str (take ()))
   in
      pickChars ""
   end

   and parseNumber () =
   let
      val i = parseInt ()
   in
      if peek () = #"e" orelse peek () = #"E" then 
         Callbacks.json_int (valOf (Int.fromString (i^parseExp())))
      else if peek () = #"." then
         let
            val f = parseFrac()

            val f' = if peek() = #"e" orelse peek() = #"E" then
                        i ^ f ^ parseExp ()
                     else i ^ f
         in
            Callbacks.json_real (valOf (Real.fromString f'))
         end
      else Callbacks.json_int (valOf (Int.fromString i))
   end

   and parseInt () =
   let
      val f =
          if peek () = #"-"
          then (take (); "~")
          else String.str (take ())
   in
      f ^ parseDigits ()
   end

   and parseDigits () = 
   let
      val r = ref ""
   in
      (while Char.isDigit (peek ()) do
         r := !r ^ String.str (take ());
       !r)
   end

   and parseFrac () =
      (consume "." ;
         "." ^ parseDigits ())

   and parseExp () =
   let
      val _ = 
         if peek () = #"e" orelse
            peek () = #"E" then take ()
         else 
            raise JSONParseError ("Invalid number", !inputPosition)

      val f = if peek () = #"-" then (take (); "~")
               else if peek () = #"+" then (take (); "")
               else ""
   in
      "e" ^ f ^ parseDigits ()
   end

   fun parse s = 
      (inputData := s ;
       inputPosition := 0 ;
       parseObject ()) handle JSONParseError (m,p) => 
         Callbacks.error_handle (m,p,!inputData)
end

structure JsonIntermAst = 
struct 
datatype ast =
         Array of ast list
         | Null
         | Float of real
         | String of string
         | Bool of bool
         | Int of int
         | Pair of (string * ast)
         | Obj of ast list
end

structure Json :> JSON = struct 
datatype json =
         Array of json list
         | Null
         | Float of real
         | String of string
         | Bool of bool
         | Int of int
         | Obj of (string * json) list

fun fromInterm (interm: JsonIntermAst.ast): json =
    case interm of
        JsonIntermAst.Array l => Array (List.map fromInterm l)
      | JsonIntermAst.Null => Null
      | JsonIntermAst.Float r => Float r
      | JsonIntermAst.String s => String s
      | JsonIntermAst.Bool b => Bool b
      | JsonIntermAst.Int i => Int i
      | JsonIntermAst.Pair (k,v) =>
        raise Fail ("JSON Parsing error. Pair of JSON found where it shouldn't. Key = " ^ k)
      | JsonIntermAst.Obj l =>
        Obj
          (List.foldl
            (fn (a, acc) =>
                case a of
                    JsonIntermAst.Pair (k, v) => (k, fromInterm v) :: acc
                  | JsonIntermAst.Array _ => raise Fail ("JSON Parsing error. Found Array in object instead of key-value pair")
                  | JsonIntermAst.Null =>  raise Fail ("JSON Parsing error. Found Null in object instead of key-value pair")
                  | JsonIntermAst.Float _ =>  raise Fail ("JSON Parsing error. Found Float in object instead of key-value pair")
                  | JsonIntermAst.String _ =>  raise Fail ("JSON Parsing error. Found String in object instead of key-value pair")
                  | JsonIntermAst.Bool _ =>  raise Fail ("JSON Parsing error. Found Bool in object instead of key-value pair")
                  | JsonIntermAst.Int _ => raise Fail ("JSON Parsing error. Found Int in object instead of key-value pair")
                  | JsonIntermAst.Obj _ => raise Fail ("JSON Parsing error. Found Obj in object instead of key-value pair")
            ) [] l)

structure StandardJsonParserCallbacks =
struct 
    type json_data = JsonIntermAst.ast
    fun json_object l = JsonIntermAst.Obj l
    fun json_pair (k,v) = JsonIntermAst.Pair (k,v)
    fun json_array l = JsonIntermAst.Array l
    fun json_value x = x
    fun json_string s = JsonIntermAst.String s
    fun json_int i = JsonIntermAst.Int i
    fun json_real r = JsonIntermAst.Float r
    fun json_bool b = JsonIntermAst.Bool b
    fun json_null () = JsonIntermAst.Null
    fun error_handle (msg,pos,data) =
        raise Fail ("Error: " ^ msg ^ " near " ^ Int.toString pos ^ " data: " ^
                    data)
end

structure MyJsonParser = JSONParser (StandardJsonParserCallbacks)

fun parse (str: string): json =
    fromInterm (MyJsonParser.parse str)
fun print (ast: json): string = 
    case ast of
        Array l => "["
                   ^ List.foldl (fn (a, acc) => acc ^ (if acc = "" then "" else ", ") ^ print a) "" l
                   ^ "]"
      | Null => "null"
      | Float r => Real.toString r
      | String s =>
        "\"" ^
        String.translate
        (fn c => if c = #"\"" then "\\\"" else Char.toString c)
        s ^
        "\""
      | Bool b => if b then "true" else "false"
      | Int i => if i >= 0
                 then (Int.toString i)
                 else "-" ^ (Int.toString (Int.abs i)) (* default printing uses ~ instead of - *)
      | Obj l => "{"
                 ^ List.foldl (fn ((k, v), acc) => acc ^ (if acc = "" then "" else ", ") ^ "\"" ^ k ^ "\": " ^ print v ) "" l
                ^ "}"
end
