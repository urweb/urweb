(* -*- mode: sml-lex -*- *)

(* Copyright (c) 2008-2009, Adam Chlipala
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - The names of contributors may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

(* Lexing info for Ur/Web programs *)

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token

val commentOut = ref (fn () => ())

local
  val commentLevel = ref 0
  val commentPos = ref 0
in
  fun reset () =
      (commentLevel := 0;
       commentPos := 0)

  fun enterComment pos =
      (if !commentLevel = 0 then
           commentPos := pos
       else
           ();
       commentLevel := !commentLevel + 1)
    
  fun exitComment () =
      (ignore (commentLevel := !commentLevel - 1);
       if !commentLevel = 0 then
           !commentOut ()
       else
           ())

  fun eof () = 
    let 
      val pos = ErrorMsg.lastLineStart ()
    in
      if !commentLevel > 0 then
          ErrorMsg.errorAt' (!commentPos, !commentPos) "Unterminated comment"
      else
          ();
      Tokens.EOF (pos, pos) 
    end
end

val strEnder = ref #"\""
val str = ref ([] : char list)
val strStart = ref 0

local
    val initSig = ref false
    val offset = ref 0
in

fun initialSig () = initSig := true

fun pos yypos = yypos - !offset

fun newline yypos =
    if !initSig then
        (initSig := false;
         offset := yypos + 1)
    else
        ErrorMsg.newline (pos yypos)

end

val xmlTag = ref ([] : string list)
val xmlString = ref true
val braceLevels = ref ([] : ((unit -> unit) * int) list)

fun pushLevel s = braceLevels := (s, 1) :: (!braceLevels)

fun enterBrace () =
    case !braceLevels of
	(s, i) :: rest => braceLevels := (s, i+1) :: rest
      | _ => ()

fun exitBrace () =
    case !braceLevels of
	(s, i) :: rest =>
	if i = 1 then
	    (braceLevels := rest;
	     s ())
	else
	    braceLevels := (s, i-1) :: rest
      | _ => ()

fun initialize () = (reset ();
                     xmlTag := [];
		     xmlString := false)


structure StringMap = BinaryMapFn(struct
                                  type ord_key = string
                                  val compare = String.compare
                                  end)

val entities = foldl (fn ((key, value), entities) => StringMap.insert (entities, key, value))
                     StringMap.empty Entities.all

fun unescape loc s =
    let
        fun process (s, acc) =
            let
                val (befor, after) = Substring.splitl (fn ch => ch <> #"&") s
            in
                if Substring.size after = 0 then
                    Substring.concat (rev (s :: acc))
                else
                    let
                        val after = Substring.slice (after, 1, NONE)
                        val (befor', after') = Substring.splitl (fn ch => ch <> #";") after
                    in
                        if Substring.size after' = 0 then
                            (ErrorMsg.errorAt' loc "Missing ';' after '&'";
                             "")
                        else
                            let
                                val pre = befor
                                val code = befor'
                                val s = Substring.slice (after', 1, NONE)

                                val special =
                                    if Substring.size code > 0 andalso Substring.sub (code, 0) = #"#"
                                       andalso CharVectorSlice.all Char.isDigit (Substring.slice (code, 1, NONE)) then
                                        let
                                            val code = Substring.string (Substring.slice (code, 1, NONE))
                                        in
                                            Option.map Utf8.encode (Int.fromString code)
                                        end
                                    else
                                        Option.map Utf8.encode (StringMap.find (entities, Substring.string code))
                            in
                                case special of
                                    NONE => (ErrorMsg.errorAt' loc ("Unsupported XML character entity "
                                                                        ^ Substring.string code);
                                             "")
                                  | SOME sp => process (s, Substring.full sp :: pre :: acc)
                            end
                    end
            end
    in
        process (Substring.full s, [])
    end

%%
%header (functor UrwebLexFn(structure Tokens : Urweb_TOKENS));
%full
%s COMMENT STRING CHAR XML XMLTAG;

id = [a-z_][A-Za-z0-9_']*;
cid = [A-Z][A-Za-z0-9_]*;
ws = [\ \t\012\r];
intconst = [0-9]+;
realconst = [0-9]+\.[0-9]*;
notags = ([^<{\n(]|(\([^\*<{\n]))+;
xcom = ([^\-]|(-[^\-]))+;
oint = [0-9][0-9][0-9];
xint = x[0-9a-fA-F][0-9a-fA-F];

%%

<INITIAL,COMMENT,XMLTAG>
      \n              => (newline yypos;
                          continue ());
<XML> \n              => (newline yypos;
                          Tokens.NOTAGS (yytext, yypos, yypos + size yytext));

<INITIAL> {ws}+       => (lex ());

<INITIAL> "(*"        => (YYBEGIN COMMENT;
                          commentOut := (fn () => YYBEGIN INITIAL);
                          enterComment (pos yypos);
                          continue ());
<XML> "(*"            => (YYBEGIN COMMENT;
                          commentOut := (fn () => YYBEGIN XML);
                          enterComment (pos yypos);
                          continue ());
<XMLTAG> "(*"         => (YYBEGIN COMMENT;
                          commentOut := (fn () => YYBEGIN XMLTAG);
                          enterComment (pos yypos);
                          continue ());
<INITIAL,XML,XMLTAG>
             "*)"     => (ErrorMsg.errorAt' (pos yypos, pos yypos) "Unbalanced comments";
			  continue ());

<COMMENT> "(*"        => (enterComment (pos yypos);
                          continue ());
<COMMENT> "*)"        => (exitComment ();
			  continue ());

<XML> "<!--" {xcom} "-->" => (continue ());

<STRING,CHAR> "\\\""  => (str := #"\"" :: !str; continue());
<STRING,CHAR> "\\'"   => (str := #"'" :: !str; continue());
<STRING,CHAR> "\\n"   => (str := #"\n" :: !str; continue());
<STRING,CHAR> "\\\\"  => (str := #"\\" :: !str; continue());
<STRING,CHAR> "\\t"   => (str := #"\t" :: !str; continue());
<STRING,CHAR> "\n"    => (newline yypos;
			  str := #"\n" :: !str; continue());
<STRING,CHAR> "\\" {oint} => (case StringCvt.scanString (Int.scan StringCvt.OCT)
                                                        (String.extract (yytext, 1, NONE)) of
                                  NONE => ErrorMsg.errorAt' (pos yypos, pos yypos) "Illegal string escape"
                                | SOME n => str := chr n :: !str;
                              continue());
<STRING,CHAR> "\\" {xint} => (case StringCvt.scanString (Int.scan StringCvt.HEX)
                                                        (String.extract (yytext, 2, NONE)) of
                                  NONE => ErrorMsg.errorAt' (pos yypos, pos yypos) "Illegal string escape"
                                | SOME n => str := chr n :: !str;
                              continue());

<INITIAL> "#\""       => (YYBEGIN CHAR; strEnder := #"\""; strStart := pos yypos; str := []; continue());

<CHAR> .              => (let
                              val ch = String.sub (yytext, 0)
                          in
                              if ch = !strEnder then
                                  let
                                      val s = String.implode (List.rev (!str))
                                  in
			              YYBEGIN INITIAL;
                                      if size s = 1 then
			                  Tokens.CHAR (String.sub (s, 0), !strStart, pos yypos + 1)
                                      else
                                          (ErrorMsg.errorAt' (yypos, yypos)
                                                             "Character constant is zero or multiple characters";
                                           continue ())
                                  end
                              else
                                  (str := ch :: !str;
                                   continue ())
                          end);

<INITIAL> "\""        => (YYBEGIN STRING; strEnder := #"\""; strStart := pos yypos; str := []; continue());
<INITIAL> "'"         => (YYBEGIN STRING; strEnder := #"'"; strStart := pos yypos; str := []; continue());

<STRING> .            => (let
                              val ch = String.sub (yytext, 0)
                          in
                              if ch = !strEnder then
                                  (if !xmlString then
			               (xmlString := false; YYBEGIN XMLTAG)
			           else
			               YYBEGIN INITIAL;
			           Tokens.STRING (String.implode (List.rev (!str)), !strStart, pos yypos + 1))
                              else
                                  (str := ch :: !str;
                                   continue ())
                          end);

<INITIAL> "<" {id} "/>"=>(let
			      val tag = String.substring (yytext, 1, size yytext - 3)
			  in
			      Tokens.XML_BEGIN_END (tag, yypos, yypos + size yytext)
			  end);
<INITIAL> "<" {id} ">"=> (let
			      val tag = String.substring (yytext, 1, size yytext - 2)
			  in
			      YYBEGIN XML;
			      xmlTag := tag :: (!xmlTag);
			      Tokens.XML_BEGIN (tag, yypos, yypos + size yytext)
			  end);
<XML> "</" {id} ">"   => (let
			      val id = String.substring (yytext, 2, size yytext - 3)
			  in
			      case !xmlTag of
			          id' :: rest =>
			          if id = id' then
				      (YYBEGIN INITIAL;
				       xmlTag := rest;
				       Tokens.XML_END (yypos, yypos + size yytext))
			          else
				      Tokens.END_TAG (id, yypos, yypos + size yytext)
			        | _ => 
			          Tokens.END_TAG (id, yypos, yypos + size yytext)
			  end);

<XML> "<" {id}        => (YYBEGIN XMLTAG;
			  Tokens.BEGIN_TAG (String.extract (yytext, 1, NONE),
					    yypos, yypos + size yytext));

<XMLTAG> "/"          => (Tokens.DIVIDE (yypos, yypos + size yytext));
<XMLTAG> ">"          => (YYBEGIN XML;
			  Tokens.GT (yypos, yypos + size yytext));

<XMLTAG> {ws}+        => (lex ());

<XMLTAG> {id}         => (Tokens.SYMBOL (yytext, yypos, yypos + size yytext));
<XMLTAG> "="          => (Tokens.EQ (yypos, yypos + size yytext));

<XMLTAG> {intconst}   => (case Int64.fromString yytext of
                            SOME x => Tokens.INT (x, yypos, yypos + size yytext)
                          | NONE   => (ErrorMsg.errorAt' (yypos, yypos)
                                       ("Expected int, received: " ^ yytext);
                                       continue ()));
<XMLTAG> {realconst}  => (case Real.fromString yytext of
                            SOME x => Tokens.FLOAT (x, yypos, yypos + size yytext)
                          | NONE   => (ErrorMsg.errorAt' (yypos, yypos)
                                       ("Expected float, received: " ^ yytext);
                                       continue ()));
<XMLTAG> "\""         => (YYBEGIN STRING;
			  xmlString := true; strEnder := #"\"";
			  strStart := yypos; str := []; continue ());

<XMLTAG> "{"          => (YYBEGIN INITIAL;
			  pushLevel (fn () => YYBEGIN XMLTAG);
			  Tokens.LBRACE (yypos, yypos + 1));
<XMLTAG> "("          => (YYBEGIN INITIAL;
			  pushLevel (fn () => YYBEGIN XMLTAG);
			  Tokens.LPAREN (yypos, yypos + 1));

<XMLTAG> .            => (ErrorMsg.errorAt' (yypos, yypos)
                          ("illegal XML tag character: \"" ^ yytext ^ "\"");
                          continue ());

<XML> "{"             => (YYBEGIN INITIAL;
			  pushLevel (fn () => YYBEGIN XML);
			  Tokens.LBRACE (yypos, yypos + 1));

<XML> {notags}        => (Tokens.NOTAGS (unescape (yypos, yypos + size yytext) yytext, yypos, yypos + size yytext));

<XML> "("             => (Tokens.NOTAGS ("(", yypos, yypos + size yytext));

<XML> .               => (ErrorMsg.errorAt' (yypos, yypos)
                          ("illegal XML character: \"" ^ yytext ^ "\"");
                          continue ());

<INITIAL> "()"        => (Tokens.UNIT (pos yypos, pos yypos + size yytext));
<INITIAL> "("         => (Tokens.LPAREN (pos yypos, pos yypos + size yytext));
<INITIAL> ")"         => (Tokens.RPAREN (pos yypos, pos yypos + size yytext));
<INITIAL> "["         => (Tokens.LBRACK (pos yypos, pos yypos + size yytext));
<INITIAL> "]"         => (Tokens.RBRACK (pos yypos, pos yypos + size yytext));
<INITIAL> "{"         => (enterBrace ();
                          Tokens.LBRACE (pos yypos, pos yypos + size yytext));
<INITIAL> "}"         => (exitBrace ();
                          Tokens.RBRACE (pos yypos, pos yypos + size yytext));

<INITIAL> "-->"       => (Tokens.KARROW (pos yypos, pos yypos + size yytext));
<INITIAL> "->"        => (Tokens.ARROW (pos yypos, pos yypos + size yytext));
<INITIAL> "==>"       => (Tokens.DKARROW (pos yypos, pos yypos + size yytext));
<INITIAL> "=>"        => (Tokens.DARROW (pos yypos, pos yypos + size yytext));
<INITIAL> "++"        => (Tokens.PLUSPLUS (pos yypos, pos yypos + size yytext));
<INITIAL> "--"        => (Tokens.MINUSMINUS (pos yypos, pos yypos + size yytext));
<INITIAL> "---"       => (Tokens.MINUSMINUSMINUS (pos yypos, pos yypos + size yytext));
<INITIAL> "^"         => (Tokens.CARET (pos yypos, pos yypos + size yytext));

<INITIAL> "&&"        => (Tokens.ANDALSO (pos yypos, pos yypos + size yytext));
<INITIAL> "||"        => (Tokens.ORELSE (pos yypos, pos yypos + size yytext));

<INITIAL> "="         => (Tokens.EQ (pos yypos, pos yypos + size yytext));
<INITIAL> "<>"        => (Tokens.NE (pos yypos, pos yypos + size yytext));
<INITIAL> "<"         => (Tokens.LT (pos yypos, pos yypos + size yytext));
<INITIAL> ">"         => (Tokens.GT (pos yypos, pos yypos + size yytext));
<INITIAL> "<="        => (Tokens.LE (pos yypos, pos yypos + size yytext));
<INITIAL> ">="        => (Tokens.GE (pos yypos, pos yypos + size yytext));
<INITIAL> ","         => (Tokens.COMMA (pos yypos, pos yypos + size yytext));
<INITIAL> ":::_"      => (Tokens.TCOLONWILD (pos yypos, pos yypos + size yytext));
<INITIAL> ":::"       => (Tokens.TCOLON (pos yypos, pos yypos + size yytext));
<INITIAL> "::_"       => (Tokens.DCOLONWILD (pos yypos, pos yypos + size yytext));
<INITIAL> "::"        => (Tokens.DCOLON (pos yypos, pos yypos + size yytext));
<INITIAL> ":"         => (Tokens.COLON (pos yypos, pos yypos + size yytext));
<INITIAL> "..."       => (Tokens.DOTDOTDOT (pos yypos, pos yypos + size yytext));
<INITIAL> "."         => (Tokens.DOT (pos yypos, pos yypos + size yytext));
<INITIAL> "$"         => (Tokens.DOLLAR (pos yypos, pos yypos + size yytext));
<INITIAL> "#"         => (Tokens.HASH (pos yypos, pos yypos + size yytext));
<INITIAL> "__"        => (Tokens.UNDERUNDER (pos yypos, pos yypos + size yytext));
<INITIAL> "_"         => (Tokens.UNDER (pos yypos, pos yypos + size yytext));
<INITIAL> "~"         => (Tokens.TWIDDLE (pos yypos, pos yypos + size yytext));
<INITIAL> "|"         => (Tokens.BAR (pos yypos, pos yypos + size yytext));
<INITIAL> "*"         => (Tokens.STAR (pos yypos, pos yypos + size yytext));
<INITIAL> "<-"        => (Tokens.LARROW (pos yypos, pos yypos + size yytext));
<INITIAL> ";"         => (Tokens.SEMI (pos yypos, pos yypos + size yytext));
<INITIAL> "!"         => (Tokens.BANG (pos yypos, pos yypos + size yytext));

<INITIAL> "+"         => (Tokens.PLUS (pos yypos, pos yypos + size yytext));
<INITIAL> "-"         => (Tokens.MINUS (pos yypos, pos yypos + size yytext));
<INITIAL> "/"         => (Tokens.DIVIDE (yypos, yypos + size yytext));
<INITIAL> "%"         => (Tokens.MOD (pos yypos, pos yypos + size yytext));
<INITIAL> "@"         => (Tokens.AT (pos yypos, pos yypos + size yytext));

<INITIAL> "con"       => (Tokens.CON (pos yypos, pos yypos + size yytext));
<INITIAL> "type"      => (Tokens.LTYPE (pos yypos, pos yypos + size yytext));
<INITIAL> "datatype"  => (Tokens.DATATYPE (pos yypos, pos yypos + size yytext));
<INITIAL> "of"        => (Tokens.OF (pos yypos, pos yypos + size yytext));
<INITIAL> "val"       => (Tokens.VAL (pos yypos, pos yypos + size yytext));
<INITIAL> "rec"       => (Tokens.REC (pos yypos, pos yypos + size yytext));
<INITIAL> "and"       => (Tokens.AND (pos yypos, pos yypos + size yytext));
<INITIAL> "fun"       => (Tokens.FUN (pos yypos, pos yypos + size yytext));
<INITIAL> "fn"        => (Tokens.FN (pos yypos, pos yypos + size yytext));
<INITIAL> "map"       => (Tokens.MAP (pos yypos, pos yypos + size yytext));
<INITIAL> "case"      => (Tokens.CASE (pos yypos, pos yypos + size yytext));
<INITIAL> "if"        => (Tokens.IF (pos yypos, pos yypos + size yytext));
<INITIAL> "then"      => (Tokens.THEN (pos yypos, pos yypos + size yytext));
<INITIAL> "else"      => (Tokens.ELSE (pos yypos, pos yypos + size yytext));


<INITIAL> "structure" => (Tokens.STRUCTURE (pos yypos, pos yypos + size yytext));
<INITIAL> "signature" => (Tokens.SIGNATURE (pos yypos, pos yypos + size yytext));
<INITIAL> "struct"    => (Tokens.STRUCT (pos yypos, pos yypos + size yytext));
<INITIAL> "sig"       => (if yypos <= 2 then initialSig () else (); Tokens.SIG (pos yypos, pos yypos + size yytext));
<INITIAL> "let"       => (Tokens.LET (pos yypos, pos yypos + size yytext));
<INITIAL> "in"        => (Tokens.IN (pos yypos, pos yypos + size yytext));
<INITIAL> "end"       => (Tokens.END (pos yypos, pos yypos + size yytext));
<INITIAL> "functor"   => (Tokens.FUNCTOR (pos yypos, pos yypos + size yytext));
<INITIAL> "where"     => (Tokens.WHERE (pos yypos, pos yypos + size yytext));
<INITIAL> "include"   => (Tokens.INCLUDE (pos yypos, pos yypos + size yytext));
<INITIAL> "open"      => (Tokens.OPEN (pos yypos, pos yypos + size yytext));
<INITIAL> "constraint"=> (Tokens.CONSTRAINT (pos yypos, pos yypos + size yytext));
<INITIAL> "constraints"=> (Tokens.CONSTRAINTS (pos yypos, pos yypos + size yytext));
<INITIAL> "export"    => (Tokens.EXPORT (pos yypos, pos yypos + size yytext));
<INITIAL> "table"     => (Tokens.TABLE (pos yypos, pos yypos + size yytext));
<INITIAL> "sequence"  => (Tokens.SEQUENCE (pos yypos, pos yypos + size yytext));
<INITIAL> "view"      => (Tokens.VIEW (pos yypos, pos yypos + size yytext));
<INITIAL> "class"     => (Tokens.CLASS (pos yypos, pos yypos + size yytext));
<INITIAL> "cookie"    => (Tokens.COOKIE (pos yypos, pos yypos + size yytext));
<INITIAL> "style"     => (Tokens.STYLE (pos yypos, pos yypos + size yytext));
<INITIAL> "task"      => (Tokens.TASK (pos yypos, pos yypos + size yytext));
<INITIAL> "policy"    => (Tokens.POLICY (pos yypos, pos yypos + size yytext));

<INITIAL> "Type"      => (Tokens.TYPE (pos yypos, pos yypos + size yytext));
<INITIAL> "Name"      => (Tokens.NAME (pos yypos, pos yypos + size yytext));
<INITIAL> "Unit"      => (Tokens.KUNIT (pos yypos, pos yypos + size yytext));

<INITIAL> "SELECT"    => (Tokens.SELECT (pos yypos, pos yypos + size yytext));
<INITIAL> "DISTINCT"  => (Tokens.DISTINCT (pos yypos, pos yypos + size yytext));
<INITIAL> "FROM"      => (Tokens.FROM (pos yypos, pos yypos + size yytext));
<INITIAL> "AS"        => (Tokens.AS (pos yypos, pos yypos + size yytext));
<INITIAL> "WHERE"     => (Tokens.CWHERE (pos yypos, pos yypos + size yytext));
<INITIAL> "SQL"       => (Tokens.SQL (pos yypos, pos yypos + size yytext));
<INITIAL> "GROUP"     => (Tokens.GROUP (pos yypos, pos yypos + size yytext));
<INITIAL> "ORDER"     => (Tokens.ORDER (pos yypos, pos yypos + size yytext));
<INITIAL> "BY"        => (Tokens.BY (pos yypos, pos yypos + size yytext));
<INITIAL> "HAVING"    => (Tokens.HAVING (pos yypos, pos yypos + size yytext));
<INITIAL> "LIMIT"     => (Tokens.LIMIT (pos yypos, pos yypos + size yytext));
<INITIAL> "OFFSET"    => (Tokens.OFFSET (pos yypos, pos yypos + size yytext));
<INITIAL> "ALL"       => (Tokens.ALL (pos yypos, pos yypos + size yytext));
<INITIAL> "SELECT1"   => (Tokens.SELECT1 (pos yypos, pos yypos + size yytext));

<INITIAL> "JOIN"      => (Tokens.JOIN (pos yypos, pos yypos + size yytext));
<INITIAL> "INNER"     => (Tokens.INNER (pos yypos, pos yypos + size yytext));
<INITIAL> "CROSS"     => (Tokens.CROSS (pos yypos, pos yypos + size yytext));
<INITIAL> "OUTER"     => (Tokens.OUTER (pos yypos, pos yypos + size yytext));
<INITIAL> "LEFT"      => (Tokens.LEFT (pos yypos, pos yypos + size yytext));
<INITIAL> "RIGHT"     => (Tokens.RIGHT (pos yypos, pos yypos + size yytext));
<INITIAL> "FULL"      => (Tokens.FULL (pos yypos, pos yypos + size yytext));

<INITIAL> "UNION"     => (Tokens.UNION (pos yypos, pos yypos + size yytext));
<INITIAL> "INTERSECT" => (Tokens.INTERSECT (pos yypos, pos yypos + size yytext));
<INITIAL> "EXCEPT"    => (Tokens.EXCEPT (pos yypos, pos yypos + size yytext));

<INITIAL> "TRUE"      => (Tokens.TRUE (pos yypos, pos yypos + size yytext));
<INITIAL> "FALSE"     => (Tokens.FALSE (pos yypos, pos yypos + size yytext));
<INITIAL> "AND"       => (Tokens.CAND (pos yypos, pos yypos + size yytext));
<INITIAL> "OR"        => (Tokens.OR (pos yypos, pos yypos + size yytext));
<INITIAL> "NOT"       => (Tokens.NOT (pos yypos, pos yypos + size yytext));

<INITIAL> "COUNT"     => (Tokens.COUNT (pos yypos, pos yypos + size yytext));
<INITIAL> "AVG"       => (Tokens.AVG (pos yypos, pos yypos + size yytext));
<INITIAL> "SUM"       => (Tokens.SUM (pos yypos, pos yypos + size yytext));
<INITIAL> "MIN"       => (Tokens.MIN (pos yypos, pos yypos + size yytext));
<INITIAL> "MAX"       => (Tokens.MAX (pos yypos, pos yypos + size yytext));

<INITIAL> "IF"        => (Tokens.CIF (pos yypos, pos yypos + size yytext));
<INITIAL> "THEN"      => (Tokens.CTHEN (pos yypos, pos yypos + size yytext));
<INITIAL> "ELSE"      => (Tokens.CELSE (pos yypos, pos yypos + size yytext));

<INITIAL> "ASC"       => (Tokens.ASC (pos yypos, pos yypos + size yytext));
<INITIAL> "DESC"      => (Tokens.DESC (pos yypos, pos yypos + size yytext));
<INITIAL> "RANDOM"    => (Tokens.RANDOM (pos yypos, pos yypos + size yytext));

<INITIAL> "INSERT"    => (Tokens.INSERT (pos yypos, pos yypos + size yytext));
<INITIAL> "INTO"      => (Tokens.INTO (pos yypos, pos yypos + size yytext));
<INITIAL> "VALUES"    => (Tokens.VALUES (pos yypos, pos yypos + size yytext));
<INITIAL> "UPDATE"    => (Tokens.UPDATE (pos yypos, pos yypos + size yytext));
<INITIAL> "SET"       => (Tokens.SET (pos yypos, pos yypos + size yytext));
<INITIAL> "DELETE"    => (Tokens.DELETE (pos yypos, pos yypos + size yytext));
<INITIAL> "NULL"      => (Tokens.NULL (pos yypos, pos yypos + size yytext));
<INITIAL> "IS"        => (Tokens.IS (pos yypos, pos yypos + size yytext));
<INITIAL> "COALESCE"  => (Tokens.COALESCE (pos yypos, pos yypos + size yytext));
<INITIAL> "LIKE"      => (Tokens.LIKE (pos yypos, pos yypos + size yytext));

<INITIAL> "CONSTRAINT"=> (Tokens.CCONSTRAINT (pos yypos, pos yypos + size yytext));
<INITIAL> "UNIQUE"    => (Tokens.UNIQUE (pos yypos, pos yypos + size yytext));
<INITIAL> "CHECK"     => (Tokens.CHECK (pos yypos, pos yypos + size yytext));
<INITIAL> "PRIMARY"   => (Tokens.PRIMARY (pos yypos, pos yypos + size yytext));
<INITIAL> "FOREIGN"   => (Tokens.FOREIGN (pos yypos, pos yypos + size yytext));
<INITIAL> "KEY"       => (Tokens.KEY (pos yypos, pos yypos + size yytext));
<INITIAL> "ON"        => (Tokens.ON (pos yypos, pos yypos + size yytext));
<INITIAL> "NO"        => (Tokens.NO (pos yypos, pos yypos + size yytext));
<INITIAL> "ACTION"    => (Tokens.ACTION (pos yypos, pos yypos + size yytext));
<INITIAL> "RESTRICT"  => (Tokens.RESTRICT (pos yypos, pos yypos + size yytext));
<INITIAL> "CASCADE"   => (Tokens.CASCADE (pos yypos, pos yypos + size yytext));
<INITIAL> "REFERENCES"=> (Tokens.REFERENCES (pos yypos, pos yypos + size yytext));

<INITIAL> "CURRENT_TIMESTAMP" => (Tokens.CURRENT_TIMESTAMP (pos yypos, pos yypos + size yytext));

<INITIAL> "CURRENT_TIMESTAMP" => (Tokens.CURRENT_TIMESTAMP (pos yypos, pos yypos + size yytext));

<INITIAL> {id}        => (Tokens.SYMBOL (yytext, pos yypos, pos yypos + size yytext));
<INITIAL> {cid}       => (Tokens.CSYMBOL (yytext, pos yypos, pos yypos + size yytext));

<INITIAL> {intconst}  => (case Int64.fromString yytext of
                              SOME x => Tokens.INT (x, pos yypos, pos yypos + size yytext)
                            | NONE   => (ErrorMsg.errorAt' (pos yypos, pos yypos)
                                                           ("Expected int, received: " ^ yytext);
                                         continue ()));
<INITIAL> {realconst} => (case Real64.fromString yytext of
                            SOME x => Tokens.FLOAT (x, pos yypos, pos yypos + size yytext)
                          | NONE   => (ErrorMsg.errorAt' (pos yypos, pos yypos)
                                       ("Expected float, received: " ^ yytext);
                                       continue ()));

<COMMENT> .           => (continue());

<INITIAL> .           => (ErrorMsg.errorAt' (pos yypos, pos yypos)
                                            ("illegal character: \"" ^ yytext ^ "\"");
                          continue ());
