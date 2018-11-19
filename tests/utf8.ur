
fun test_fn_both_sides [a ::: Type] (_ : eq a) (_ : show a) (f : unit -> a) (expected : a) (testname : string) : xbody =
<xml>
  <p>Server side test: {[testname]}</p>
  <pre>{[assert (f () = expected) "False" testname "True" ]}</pre>	
  <active code={let
		    val computed = f ()
		    val msgErr = "False " ^ testname ^ ": expected " ^ (show expected) ^ " but is " ^ (show computed)
		    val r = assert (computed = expected) msgErr testname "True"
		in
		    return <xml><p>Client side test: {[testname]}</p><pre>{[r]}</pre></xml>
		end}>
</active>
</xml>

fun test_fn_sside [a ::: Type] (_ : eq a) (_ : show a) (f : unit -> a) (expected : a) (testname : string) : xbody =
    <xml>
      <p>Server side test: {[testname]}</p>
      <pre>{[assert (f () = expected) "False" testname "True" ]}</pre>
    </xml>

fun substrings () : transaction page =

    return <xml>
      <body>

	{test_fn_both_sides (fn _ => substring "abc" 0 3) "abc" "substrings 1"}
	{test_fn_both_sides (fn _ => substring "abc" 1 2) "bc" "substrings 2"}
	{test_fn_both_sides (fn _ => substring "abc" 2 1) "c" "substrings 3"}
	{test_fn_both_sides (fn _ => substring "ábó" 0 3) "ábó" "substrings 4"}
	{test_fn_both_sides (fn _ => substring "ábó" 1 2) "bó" "substrings 5"}
	{test_fn_both_sides (fn _ => substring "ábó" 2 1) "ó" "substrings 6"}
	{test_fn_both_sides (fn _ => substring "ábó" 0 2) "áb" "substrings 7"}
	{test_fn_both_sides (fn _ => substring "ábó" 0 1) "á" "substrings 8"}

	{test_fn_both_sides (fn _ => substring "" 0 0) "" "substrings 9"}
      </body>
    </xml>


fun strlens () : transaction page = return <xml>
  <body> 
    {test_fn_both_sides (fn _ => strlen "abc") 3 "strlen 1"}
    {test_fn_both_sides (fn _ => strlen "çbc") 3 "strlen 2"}
    {test_fn_both_sides (fn _ => strlen "çãc") 3 "strlen 3"}
    {test_fn_both_sides (fn _ => strlen "çãó") 3 "strlen 4"}
    {test_fn_both_sides (fn _ => strlen "ç") 1 "strlen 5"}
    {test_fn_both_sides (fn _ => strlen "c") 1 "strlen 6"}
    {test_fn_both_sides (fn _ => strlen "") 0 "strlen 7"}
    {test_fn_both_sides (fn _ => strlen "が") 1 "strlen 8"}
    {test_fn_both_sides (fn _ => strlen "漢") 1 "strlen 9"}
    {test_fn_both_sides (fn _ => strlen "カ") 1 "strlen 10"}
    {test_fn_both_sides (fn _ => strlen "وظيفية") 6 "strlen 11"}
    {test_fn_both_sides (fn _ => strlen "函數") 2 "strlen 12"}
    {test_fn_both_sides (fn _ => strlen "Функциональное") 14 "strlen 13"}
    
  </body>
  </xml>
				       
fun strlenGens () : transaction page = return <xml>
  <body>
    {test_fn_both_sides (fn _ => strlenGe "" 1) False "strlenGe 1"}
    {test_fn_both_sides (fn _ => strlenGe "" 0) True "strlenGe 2"}
    {test_fn_both_sides (fn _ => strlenGe "aba" 4) False "strlenGe 3"}
    {test_fn_both_sides (fn _ => strlenGe "aba" 3) True "strlenGe 4"}
    {test_fn_both_sides (fn _ => strlenGe "aba" 2) True "strlenGe 5"}

    {test_fn_both_sides (fn _ => strlenGe "àçá" 4) False "strlenGe 6"}
    {test_fn_both_sides (fn _ => strlenGe "àçá" 3) True "strlenGe 7"}
    {test_fn_both_sides (fn _ => strlenGe "àçá" 2) True "strlenGe 8"}
    
  </body>
  </xml>

type clen = { S : string, L : int }

val clen_eq : eq clen = mkEq (fn a b =>
			      a.S = b.S && a.L = b.L)

val clen_show : show clen = mkShow (fn a =>
				   "{S = " ^ a.S ^ ", L = " ^ (show a.L) ^ "}")

fun strcats () : transaction page =
    let
	fun test_cat_and_len n a b expS expL =
	    test_fn_both_sides (fn _ => let val c = strcat a b in {S = c, L = strlen c} end) {S=expS, L=expL} ("strcat " ^ (show n))
    in
	return <xml>
	  <body>
	    {test_cat_and_len 1 "" "" "" 0}
	    
	    {test_cat_and_len 2 "aa" "bb" "aabb" 4}
	    {test_cat_and_len 3 "" "bb" "bb" 2}
	    {test_cat_and_len 4 "aa" "" "aa" 2}

	    {test_cat_and_len 5 "àà" "áá" "ààáá" 4}
	    {test_cat_and_len 6 "" "áá" "áá" 2}
	    {test_cat_and_len 7 "àà" "" "àà" 2}

	    {test_cat_and_len 8 "函數" "ãã" "函數ãã" 4}
	     
	  </body>
	</xml>
end

fun strsubs () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => strsub "abàç" 0) #"a" "strsub 1"}
	{test_fn_both_sides (fn _ => strsub "abàç" 1) #"b" "strsub 2"}
	{test_fn_both_sides (fn _ => strsub "àb" 0) (strsub "à" 0) "strsub 3"}
	{test_fn_both_sides (fn _ => strsub "abàç" 2) (strsub "à" 0) "strsub 4"}
	{test_fn_both_sides (fn _ => strsub "abàç" 3) (strsub "ç" 0) "strsub 5"}
      </body>
      </xml>

fun strsuffixs () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => strsuffix "abàç" 0) "abàç" "strsuffix 1"}
	{test_fn_both_sides (fn _ => strsuffix "abàç" 1) "bàç" "strsuffix 2"}
	{test_fn_both_sides (fn _ => strsuffix "abàç" 2) "àç" "strsuffix 3"}
	{test_fn_both_sides (fn _ => strsuffix "abàç" 3) "ç" "strsuffix 4"}
      </body>
    </xml>

fun strchrs () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => strchr "abàç" #"c") None "strchr 1"}
	{test_fn_both_sides (fn _ => strchr "abàç" #"a") (Some "abàç") "strchr 2"}
	{test_fn_both_sides (fn _ => strchr "abàç" #"b") (Some "bàç") "strchr 3"}
	{test_fn_both_sides (fn _ => strchr "abàç" (strsub "à" 0)) (Some "àç") "strchr 4"}
	{test_fn_both_sides (fn _ => strchr "abàç" (strsub "ç" 0)) (Some "ç") "strchr 5"}	    
      </body>
    </xml>

fun strindexs () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => strindex "abàç" #"c") None "strindex 1"}
	{test_fn_both_sides (fn _ => strindex "abàç" #"a") (Some 0) "strindex 2"}
	{test_fn_both_sides (fn _ => strindex "abàç" #"b") (Some 1) "strindex 3"}
	{test_fn_both_sides (fn _ => strindex "abàç" (strsub "à" 0)) (Some 2) "strindex 4"}
	{test_fn_both_sides (fn _ => strindex "abàç" (strsub "ç" 0)) (Some 3) "strindex 5"}	
      </body>
    </xml>
    

fun strsindexs () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => strsindex "abàç" "") (Some 0) "strsindex 1"}
	{test_fn_both_sides (fn _ => strsindex "abàç" "abàç") (Some 0) "strsindex 2"}
	{test_fn_both_sides (fn _ => strsindex "abàç" "abàc") None "strsindex 3"}
	{test_fn_both_sides (fn _ => strsindex "abàç" "bàç") (Some 1) "strsindex 4"}
	{test_fn_both_sides (fn _ => strsindex "abàç" "bàc") None "strsindex 5"}
	{test_fn_both_sides (fn _ => strsindex "abàç" "àç") (Some 2) "strsindex 6"}
	{test_fn_both_sides (fn _ => strsindex "abàç" "àc") None "strsindex 7"}
	{test_fn_both_sides (fn _ => strsindex "abàç" "ç") (Some 3) "strsindex 8"}
	{test_fn_both_sides (fn _ => strsindex "abàç" "c") None "strsindex 9"}
      </body>
    </xml>
	
fun strcspns () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => strcspn "abàç" "") 4 "strcspn 1"}
	{test_fn_both_sides (fn _ => strcspn "abàç" "abàç") 0 "strcspn 2"}
	{test_fn_both_sides (fn _ => strcspn "abàç" "a") 0 "strcspn 3"}
	{test_fn_both_sides (fn _ => strcspn "abàç" "bàç") 1 "strcspn 4"}
	{test_fn_both_sides (fn _ => strcspn "abàç" "àç") 2 "strcspn 5"}
	{test_fn_both_sides (fn _ => strcspn "abàç" "ç") 3 "strcspn 6"}	
      </body>
      </xml>

fun str1s () : transaction page = return <xml>
  <body>
    {test_fn_both_sides (fn _ => str1 #"a") "a" "str1 1"}
    {test_fn_both_sides (fn _ => str1 (strsub "à" 0)) "à" "str1 2"}
    {test_fn_both_sides (fn _ => str1 (strsub "aá" 1)) "á" "str1 3"}    
  </body>
  </xml>

fun isalnums () : transaction page = return <xml>
  <body>
    {test_fn_both_sides (fn _ => isalnum #"a") True "isalnum 1"}
    {test_fn_both_sides (fn _ => isalnum #"a") True "isalnum 2"}
    {test_fn_both_sides (fn _ => isalnum (strsub "à" 0)) True "isalnum 3"}
    {test_fn_both_sides (fn _ => isalnum #"A") True "isalnum 4"}
    {test_fn_both_sides (fn _ => isalnum (strsub "À" 0)) True "isalnum 5"}
    {test_fn_both_sides (fn _ => isalnum #"1") True "isalnum 6"}
    {test_fn_both_sides (fn _ => not (isalnum #"!")) True "isalnum 7"}
    {test_fn_both_sides (fn _ => not (isalnum #"#")) True "isalnum 8"}
    {test_fn_both_sides (fn _ => not (isalnum #" ")) True "isalnum 9"}
  </body>
</xml>

fun isalphas () : transaction page = return <xml>
  <body>
    {test_fn_both_sides (fn _ => isalpha #"a") True "isalpha 1"}
    {test_fn_both_sides (fn _ => isalpha (strsub "à" 0)) True "isalpha 2"}
    {test_fn_both_sides (fn _ => isalpha #"A") True "isalpha 3"}
    {test_fn_both_sides (fn _ => isalpha (strsub "À" 0)) True "isalpha 4"}
    {test_fn_both_sides (fn _ => not (isalpha #"1")) True "isalpha 5"}
    {test_fn_both_sides (fn _ => not (isalpha #"!")) True "isalpha 6"}
    {test_fn_both_sides (fn _ => not (isalpha #"#")) True "isalpha 7"}
    {test_fn_both_sides (fn _ => not (isalpha #" ")) True "isalpha 8"}
  </body>
</xml>

fun isblanks () : transaction page = 
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => not (isblank #"a")) True "isblank 1"}
	{test_fn_both_sides (fn _ => not (isblank (strsub "à" 0))) True "isblank 2"}
	{test_fn_both_sides (fn _ => not (isblank #"A")) True "isblank 3"}
	{test_fn_both_sides (fn _ => not (isblank (strsub "À" 0))) True "isblank 4"}
	{test_fn_both_sides (fn _ => not (isblank #"1")) True "isblank 5"}
	{test_fn_both_sides (fn _ => not (isblank #"!")) True "isblank 6"}
	{test_fn_both_sides (fn _ => not (isblank #"#")) True "isblank 7"}
	{test_fn_both_sides (fn _ => isblank #" ") True "isblank 8"}
	{test_fn_both_sides (fn _ => isblank #"\t") True "isblank 9"}
	{test_fn_both_sides (fn _ => not (isblank #"\n")) True "isblank 10"}
      </body>
    </xml>

fun iscntrls () : transaction page =
    return <xml>
      <body>
	{test_fn_sside (fn _ => not (iscntrl #"a")) True "iscntrl 1"}
	{test_fn_sside (fn _ => not (iscntrl (strsub "à" 0))) True "iscntrl 2"}
	{test_fn_sside (fn _ => not (iscntrl #"A")) True "iscntrl 3"}
	{test_fn_sside (fn _ => not (iscntrl (strsub "À" 0))) True "iscntrl 4"}
	{test_fn_sside (fn _ => not (iscntrl #"1")) True "iscntrl 5"}
	{test_fn_sside (fn _ => not (iscntrl #"!")) True "iscntrl 6"}
	{test_fn_sside (fn _ => not (iscntrl #"#")) True "iscntrl 7"}
	{test_fn_sside (fn _ => not (iscntrl #" ")) True "iscntrl 8"}
	{test_fn_sside (fn _ => iscntrl #"\t") True "iscntrl 9"}
	{test_fn_sside (fn _ => iscntrl #"\n") True "iscntrl 10"}
      </body>
      </xml>

fun isdigits () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => not (isdigit #"a")) True "isdigit 1"}
	{test_fn_both_sides (fn _ => not (isdigit (strsub "à" 0))) True "isdigit 2"}
	{test_fn_both_sides (fn _ => not (isdigit #"A")) True "isdigit 3"}
	{test_fn_both_sides (fn _ => not (isdigit (strsub "À" 0))) True "isdigit 4"}
	{test_fn_both_sides (fn _ => isdigit #"1") True "isdigit 5"}
	{test_fn_both_sides (fn _ => not (isdigit #"!")) True "isdigit 6"}
	{test_fn_both_sides (fn _ => not (isdigit #"#")) True "isdigit 7"}
	{test_fn_both_sides (fn _ => not (isdigit #" ")) True "isdigit 8"}
	{test_fn_both_sides (fn _ => not (isdigit #"\t")) True "isdigit 9"}
	{test_fn_both_sides (fn _ => not (isdigit #"\n")) True "isdigit 10"}
      </body>
    </xml>

fun isgraphs () : transaction page =
    return <xml>
      <body>
	{test_fn_sside (fn _ => isgraph #"a") True "isgraph 1"}
	{test_fn_sside (fn _ => isgraph (strsub "à" 0)) True "isgraph 2"}
	{test_fn_sside (fn _ => isgraph #"A") True "isgraph 3"}
	{test_fn_sside (fn _ => isgraph (strsub "À" 0)) True "isgraph 4"}
	{test_fn_sside (fn _ => isgraph #"1") True "isgraph 5"}
	{test_fn_sside (fn _ => isgraph #"!") True "isgraph 6"}
	{test_fn_sside (fn _ => isgraph #"#") True "isgraph 7"}
	{test_fn_sside (fn _ => not (isgraph #" ")) True "isgraph 8"}
	{test_fn_sside (fn _ => not (isgraph #"\t")) True "isgraph 9"}
	{test_fn_sside (fn _ => not (isdigit #"\n")) True "isgraph 10"}
      </body>
    </xml>

fun islowers () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => islower #"a") True "islower 1"}
	{test_fn_both_sides (fn _ => islower (strsub "à" 0)) True "islower 2"}
	{test_fn_both_sides (fn _ => not (islower #"A")) True "islower 3"}
	{test_fn_both_sides (fn _ => not (islower (strsub "À" 0))) True "islower 4"}
	{test_fn_both_sides (fn _ => not (islower #"1")) True "islower 5"}
	{test_fn_both_sides (fn _ => not (islower #"!")) True "islower 6"}
	{test_fn_both_sides (fn _ => not (islower #"#")) True "islower 7"}
	{test_fn_both_sides (fn _ => not (islower #" ")) True "islower 8"}
	{test_fn_both_sides (fn _ => not (islower #"\t")) True "islower 9"}
	{test_fn_both_sides (fn _ => not (islower #"\n")) True "islower 10"}
      </body>
      </xml>

fun isprints () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => isprint #"a") True "isprint 1"}
	{test_fn_both_sides (fn _ => isprint (strsub "à" 0)) True "isprint 2"}
	{test_fn_both_sides (fn _ => isprint #"A") True "isprint 3"}
	{test_fn_both_sides (fn _ => isprint (strsub "À" 0)) True "isprint 4"}
	{test_fn_both_sides (fn _ => isprint #"1") True "isprint 5"}
	{test_fn_both_sides (fn _ => isprint #"!") True "isprint 6"}
	{test_fn_both_sides (fn _ => isprint #"#") True "isprint 7"}
	{test_fn_both_sides (fn _ => isprint #" ") True "isprint 8"}
	{test_fn_both_sides (fn _ => not (isprint #"\t")) True "isprint 9"}
	{test_fn_both_sides (fn _ => not (isprint #"\n")) True "isprint 10"}
      </body>
      </xml>

fun ispuncts () : transaction page =
    return <xml>
      <body>
	{test_fn_sside (fn _ => not (ispunct #"a")) True "ispunct 1"}
	{test_fn_sside (fn _ => not (ispunct (strsub "à" 0))) True "ispunct 2"}
	{test_fn_sside (fn _ => not (ispunct #"A")) True "ispunct 3"}
	{test_fn_sside (fn _ => not (ispunct (strsub "À" 0))) True "ispunct 4"}
	{test_fn_sside (fn _ => not (ispunct #"1")) True "ispunct 5"}
	{test_fn_sside (fn _ => ispunct #"!") True "ispunct 6"}
	{test_fn_sside (fn _ => ispunct #"#") True "ispunct 7"}
	{test_fn_sside (fn _ => not (ispunct #" ")) True "ispunct 8"}
	{test_fn_sside (fn _ => not (isprint #"\t")) True "ispunct 9"}
	{test_fn_sside (fn _ => not (isprint #"\n")) True "ispunct 10"}
      </body>
      </xml>

fun isspaces () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => not (isspace #"a")) True "isspace 1"}
	{test_fn_both_sides (fn _ => not (isspace (strsub "à" 0))) True "isspace 2"}
	{test_fn_both_sides (fn _ => not (isspace #"A")) True "isspace 3"}
	{test_fn_both_sides (fn _ => not (isspace (strsub "À" 0))) True "isspace 4"}
	{test_fn_both_sides (fn _ => not (isspace #"1")) True "isspace 5"}
	{test_fn_both_sides (fn _ => not (isspace #"!")) True "isspace 6"}
	{test_fn_both_sides (fn _ => not (isspace #"#")) True "isspace 7"}
	{test_fn_both_sides (fn _ => isspace #" ") True "isspace 8"}
	{test_fn_both_sides (fn _ => isspace #"\t") True "isspace 9"}
	{test_fn_both_sides (fn _ => isspace #"\n") True "isspace 10"}
      </body>
    </xml>
    
fun isuppers () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => not (isupper #"a")) True "isupper 1"}
	{test_fn_both_sides (fn _ => not (isupper (strsub "à" 0))) True "isupper 2"}
	{test_fn_both_sides (fn _ => isupper #"A") True "isupper 3"}
	{test_fn_both_sides (fn _ => isupper (strsub "À" 0)) True "isupper 4"}
	{test_fn_both_sides (fn _ => not (isupper #"1")) True "isupper 5"}
	{test_fn_both_sides (fn _ => not (isupper #"!")) True "isupper 6"}
	{test_fn_both_sides (fn _ => not (isupper #"#")) True "isupper 7"}
	{test_fn_both_sides (fn _ => not (isupper #" ")) True "isupper 8"}
	{test_fn_both_sides (fn _ => not (isupper #"\t")) True "isupper 9"}
	{test_fn_both_sides (fn _ => not (isupper #"\n")) True "isupper 10"}
      </body>
      </xml>

fun isxdigits () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => isxdigit #"a") True "isxdigit 1"}
	{test_fn_both_sides (fn _ => not (isxdigit (strsub "à" 0))) True "isxdigit 2"}
	{test_fn_both_sides (fn _ => isxdigit #"A") True "isxdigit 3"}
	{test_fn_both_sides (fn _ => not (isxdigit (strsub "À" 0))) True "isxdigit 4"}
	{test_fn_both_sides (fn _ => isxdigit #"1") True "isxdigit 5"}
	{test_fn_both_sides (fn _ => not (isxdigit #"!")) True "isxdigit 6"}
	{test_fn_both_sides (fn _ => not (isxdigit #"#")) True "isxdigit 7"}
	{test_fn_both_sides (fn _ => not (isxdigit #" ")) True "isxdigit 8"}
	{test_fn_both_sides (fn _ => not (isxdigit #"\t")) True "isxdigit 9"}
	{test_fn_both_sides (fn _ => not (isxdigit #"\n")) True "isxdigit 10"}
      </body>
      </xml>

fun tolowers () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => tolower #"A") #"a" "tolower 1"}
	{test_fn_both_sides (fn _ => tolower #"a") #"a" "tolower 2"}
	{test_fn_both_sides (fn _ => tolower (strsub "á" 0)) (strsub "á" 0) "tolower 3"}
	{test_fn_both_sides (fn _ => tolower (strsub "Á" 0)) (strsub "á" 0) "tolower 4"}
	{test_fn_both_sides (fn _ => tolower #"1") #"1" "tolower 5"}
      </body>
    </xml>
    
fun touppers () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => toupper #"A") #"A" "toupper 1"}
	{test_fn_both_sides (fn _ => toupper #"a") #"A" "toupper 2"}
	{test_fn_both_sides (fn _ => toupper (strsub "á" 0)) (strsub "Á" 0) "toupper 3"}
	{test_fn_both_sides (fn _ => toupper (strsub "Á" 0)) (strsub "Á" 0) "toupper 4"}
	{test_fn_both_sides (fn _ => toupper #"1") #"1" "toupper 5"}	
      </body>
      </xml>

fun ord_and_chrs () : transaction page =
    return <xml>
      <body>
	{test_fn_both_sides (fn _ => chr (ord #"A")) #"A" "ord => chr 1"}
	{test_fn_both_sides (fn _ => chr (ord #"a")) #"a" "ord => chr 2"}
	{test_fn_both_sides (fn _ => chr (ord (strsub "á" 0))) (strsub "á" 0) "ord => chr 3"}
	{test_fn_both_sides (fn _ => chr (ord (strsub "Á" 0))) (strsub "Á" 0) "ord => chr 4"}
	{test_fn_both_sides (fn _ => chr (ord #"1")) #"1" "ord => chr 5"}
	{test_fn_both_sides (fn _ => chr (ord #"\n")) #"\n" "ord => chr 6"}
	{test_fn_both_sides (fn _ => chr (ord (strsub "が" 0))) (strsub "が" 0) "ord => chr 7"}
	{test_fn_both_sides (fn _ => chr (ord (strsub "漢" 0))) (strsub "漢" 0) "ord => chr 8"}
	{test_fn_both_sides (fn _ => chr (ord (strsub "カ" 0))) (strsub "カ" 0) "ord => chr 9"}	
      </body>
      </xml>

table t : { Id : int, Text : string }


fun test_db () : transaction page =
    let
	val s1 = "abc"
	val s2 = "çãó"
	val s3 = "が"
	val s4 = "漢"
	val s5 = "カ"
	val s6 = "وظيفية"

	fun test_str_and_len n c expS expL =
	    test_fn_both_sides (fn _ => {S = c, L = strlen c}) {S=expS, L=expL} ("test_db " ^ (show n))
		 
    in
    dml (INSERT INTO t (Id, Text) VALUES({[1]}, {[s1]}));
    t1 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 1);

    dml (INSERT INTO t (Id, Text) VALUES({[2]}, {[s2]}));
    t2 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 2);

    dml (INSERT INTO t (Id, Text) VALUES({[3]}, {[s3]}));
    t3 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 3);

    dml (INSERT INTO t (Id, Text) VALUES({[4]}, {[s4]}));
    t4 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 4);

    dml (INSERT INTO t (Id, Text) VALUES({[5]}, {[s5]}));
    t5 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 5);

    dml (INSERT INTO t (Id, Text) VALUES({[6]}, {[s6]}));
    t6 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 6);

    return <xml>
      <body>
	{test_str_and_len 1 t1.T.Text s1 (strlen s1)}
	{test_str_and_len 2 t2.T.Text s2 (strlen s2)}
	{test_str_and_len 3 t3.T.Text s3 (strlen s3)}
	{test_str_and_len 4 t4.T.Text s4 (strlen s4)}
	{test_str_and_len 5 t5.T.Text s5 (strlen s5)}
	{test_str_and_len 6 t6.T.Text s6 (strlen s6)}
      </body>
      </xml>
    end

fun index () : transaction page =
    return <xml>
      <body>
	<a link={substrings ()}>substrings</a>
	<a link={strlens ()}>strlens</a>
	<a link={strlenGens ()}>strlenGens</a>
	<a link={strcats ()}>strcats</a>
	<a link={strsubs ()}>strsubs</a>
	<a link={strsuffixs ()}>strsuffixs</a>
	<a link={strchrs ()}>strchrs</a>
	<a link={strindexs ()}>strindexs</a>
	<a link={strsindexs ()}>strsindexs</a>
	<a link={strcspns ()}>strcspns</a>
	<a link={str1s ()}>str1s</a>
	<a link={isalnums ()}>isalnums</a>
	<a link={isalphas ()}>isalphas</a>
	<a link={isblanks ()}>isblanks</a>
	<a link={iscntrls ()}>iscntrls</a>
	<a link={isdigits ()}>isdigits</a>
	<a link={isgraphs ()}>isgraphs</a>
	<a link={islowers ()}>islowers</a>
	<a link={isprints ()}>isprints</a>
	<a link={ispuncts ()}>ispuncts</a>
	<a link={isspaces ()}>isspaces</a>
	<a link={isuppers ()}>isuppers</a>
	<a link={isxdigits ()}>isxdigits</a>
	<a link={tolowers ()}>tolowers</a>
	<a link={touppers ()}>touppers</a>
	<a link={ord_and_chrs ()}>ord_and_chrs</a>
	<a link={test_db ()}>test_db</a>
      </body>
      </xml>
