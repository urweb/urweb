fun substrings () : transaction page = return <xml>
  <body>
    <pre>{[substring "abc" 0 3]}</pre>   
    <pre>{[substring "abc" 1 2]}</pre>
    <pre>{[substring "abc" 2 1]}</pre>
    <pre>{[substring "ábó" 0 3]}</pre>    
    <pre>{[substring "ábó" 1 2]}</pre>
    <pre>{[substring "ábó" 2 1]}</pre>    
    <pre>{[substring "çãó" 0 3]}</pre>
    <pre>{[substring "çãó" 1 2]}</pre>
    <pre>{[substring "çãó" 2 1]}</pre>
    <pre>{[substring "çãó" 2 0]}</pre>
    <pre>{[substring "" 0 0]}</pre>
  </body>
</xml>

fun strlens () : transaction page = return <xml>
  <body>
    <pre>{[strlen "abc"]}</pre>
    <pre>{[strlen "çbc"]}</pre>
    <pre>{[strlen "çãc"]}</pre>
    <pre>{[strlen "çãó"]}</pre>
    <pre>{[strlen "ç"]}</pre>
    <pre>{[strlen "c"]}</pre>
    <pre>{[strlen ""]}</pre>
    <pre>{[strlen "が"]}</pre>
    <pre>{[strlen "漢"]}</pre>
    <pre>{[strlen "カ"]}</pre>
    <pre>{[strlen "وظيفية"]}</pre>
    <pre>{[strlen "函數"]}</pre>
    <pre>{[strlen "Функциональное"]}</pre>
  </body>
  </xml>
				       
fun strlenGens () : transaction page = return <xml>
  <body>
    <pre>{[strlenGe "" 1]}</pre>
    <pre>{[strlenGe "" 0]}</pre>
    <pre>{[strlenGe "aba" 4]}</pre>
    <pre>{[strlenGe "aba" 3]}</pre>
    <pre>{[strlenGe "aba" 2]}</pre>
    <pre>{[strlenGe "áçà" 4]}</pre>
    <pre>{[strlenGe "áçà" 3]}</pre>
    <pre>{[strlenGe "áçà" 2]}</pre>
    
  </body>
  </xml>

fun strcats () : transaction page =
    let
	fun catAndLen a b =
	    <xml>
	      <pre>{[strcat a b]}</pre>
	      <pre>{[strlen (strcat a b)]}</pre>
	    </xml>
    in
	return <xml>
	  <body>
	    {catAndLen "" ""}
	    {catAndLen "aa" "bb"}
	    {catAndLen "" "bb"}
	    {catAndLen "aa" ""}
	    {catAndLen "àà" "áá"}
	    {catAndLen "" "áá"}
	    {catAndLen "àà" ""}	    
	  </body>
	</xml>
end

fun strsubs () : transaction page =
    return <xml>
      <body>
	<pre>{[strsub "abàç" 0]}</pre>
	<pre>{[strsub "abàç" 1]}</pre>
	<pre>{[strsub "abàç" 2]}</pre>
	<pre>{[strsub "abàç" 3]}</pre>
      </body>
      </xml>

fun strsuffixs () : transaction page =
    return <xml>
      <body>
	<pre>{[strsuffix "abàç" 0]}</pre>
	<pre>{[strsuffix "abàç" 1]}</pre>
	<pre>{[strsuffix "abàç" 2]}</pre>
	<pre>{[strsuffix "abàç" 3]}</pre>
      </body>
    </xml>

fun strchrs () : transaction page =
    let
	fun optToStr ms =
	    case ms of
		None => "None"
	      | Some s => "Some \"" ^ s ^ "\""

    in
	return <xml>
	  <body>
	    <pre>{[optToStr (strchr "abàç" #"c")]}</pre>
	    <pre>{[optToStr (strchr "abàç" #"a")]}</pre>
	    <pre>{[optToStr (strchr "abàç" #"b")]}</pre>
	    <pre>{[optToStr (strchr "abàç" (strsub "à" 0))]}</pre>
	    <pre>{[optToStr (strchr "abàç" (strsub "ç" 0))]}</pre>
	  </body>
	</xml>
    end

fun strindexs () : transaction page =
    let
	fun optToStr ms =
	    case ms of
		None => "None"
	      | Some s => "Some " ^ (show s)

    in
	return <xml>
	  <body>
	    <pre>{[optToStr (strindex "abàç" #"c")]}</pre>
	    <pre>{[optToStr (strindex "abàç" #"a")]}</pre>
	    <pre>{[optToStr (strindex "abàç" #"b")]}</pre>
	    <pre>{[optToStr (strindex "abàç" (strsub "à" 0))]}</pre>
	    <pre>{[optToStr (strindex "abàç" (strsub "ç" 0))]}</pre>
	  </body>
	</xml>
    end

fun strsindexs () : transaction page =
    let
	fun optToStr ms =
	    case ms of
		None => "None"
	      | Some s => "Some " ^ (show s)

    in
	return <xml>
	  <body>
	    <pre>{[optToStr (strsindex "abàç" "")]}</pre>
	    <pre>{[optToStr (strsindex "abàç" "abàç")]}</pre>
	    <pre>{[optToStr (strsindex "abàç" "abàc")]}</pre>
	    <pre>{[optToStr (strsindex "abàç" "bàç")]}</pre>
	    <pre>{[optToStr (strsindex "abàç" "bàc")]}</pre>
	    <pre>{[optToStr (strsindex "abàç" "àç")]}</pre>
	    <pre>{[optToStr (strsindex "abàç" "àc")]}</pre>
	    <pre>{[optToStr (strsindex "abàç" "ac")]}</pre>
	    <pre>{[optToStr (strsindex "abàç" "ç")]}</pre>
	  </body>
	</xml>
    end
	
fun strcspns () : transaction page =
    return <xml>
      <body>
	<pre>{[strcspn "abàç" ""]}</pre>
	<pre>{[strcspn "abàç" "abàç"]}</pre>
	<pre>{[strcspn "abàç" "a"]}</pre>
	<pre>{[strcspn "abàç" "bàç"]}</pre>
	<pre>{[strcspn "abàç" "àç"]}</pre>
	<pre>{[strcspn "abàç" "ç"]}</pre>
      </body>
      </xml>

fun str1s () : transaction page = return <xml>
  <body>
    <pre>{[str1 #"a"]}</pre>
    <pre>{[str1 (strsub "à" 0)]}</pre>
    <pre>{[str1 (strsub "aá" 1)]}</pre>
  </body>
  </xml>

fun isalnums () : transaction page = return <xml>
  <body>
    <pre>{[isalnum #"a"]}</pre>
    <pre>{[isalnum (strsub "à" 0)]}</pre>
    <pre>{[isalnum #"A"]}</pre>
    <pre>{[isalnum (strsub "À" 0)]}</pre>
    <pre>{[isalnum #"1"]}</pre>
    <pre>{[not (isalnum #"!")]}</pre>
    <pre>{[not (isalnum #"#")]}</pre>
    <pre>{[not (isalnum #" ")]}</pre>
  </body>
</xml>

fun isalphas () : transaction page = return <xml>
  <body>
    <pre>{[isalpha #"a"]}</pre>
    <pre>{[isalpha (strsub "à" 0)]}</pre>
    <pre>{[isalpha #"A"]}</pre>
    <pre>{[isalpha (strsub "À" 0)]}</pre>
    <pre>{[not (isalpha #"1")]}</pre>
    <pre>{[not (isalpha #"!")]}</pre>
    <pre>{[not (isalpha #"#")]}</pre>
    <pre>{[not (isalpha #" ")]}</pre>
  </body>
</xml>

fun isblanks () : transaction page = 
    return <xml>
      <body>
	<pre>{[not (isblank #"a")]}</pre>
	<pre>{[not (isblank (strsub "à" 0))]}</pre>
	<pre>{[not (isblank #"A")]}</pre>
	<pre>{[not (isblank (strsub "À" 0))]}</pre>
	<pre>{[not (isblank #"1")]}</pre>
	<pre>{[not (isblank #"!")]}</pre>
	<pre>{[not (isblank #"#")]}</pre>
	<pre>{[isblank #" "]}</pre>
	<pre>{[isblank #"\t"]}</pre>
	<pre>{[not (isblank #"\n")]}</pre>
      </body>
    </xml>

fun iscntrls () : transaction page =
    return <xml>
      <body>
	<pre>{[not (iscntrl #"a")]}</pre>
	<pre>{[not (iscntrl (strsub "à" 0))]}</pre>
	<pre>{[not (iscntrl #"A")]}</pre>
	<pre>{[not (iscntrl (strsub "À" 0))]}</pre>
	<pre>{[not (iscntrl #"1")]}</pre>
	<pre>{[not (iscntrl #"!")]}</pre>
	<pre>{[not (iscntrl #"#")]}</pre>
	<pre>{[not (iscntrl #" ")]}</pre>
	<pre>{[iscntrl #"\t"]}</pre>
	<pre>{[iscntrl #"\n"]}</pre>
      </body>
      </xml>

fun isdigits () : transaction page =
    return <xml>
      <body>
	<pre>{[not (isdigit #"a")]}</pre>
	<pre>{[not (isdigit (strsub "à" 0))]}</pre>
	<pre>{[not (isdigit #"A")]}</pre>
	<pre>{[not (isdigit (strsub "À" 0))]}</pre>
	<pre>{[isdigit #"1"]}</pre>
	<pre>{[not (isdigit #"!")]}</pre>
	<pre>{[not (isdigit #"#")]}</pre>
	<pre>{[not (isdigit #" ")]}</pre>
	<pre>{[not (isdigit #"\t")]}</pre>
	<pre>{[not (isdigit #"\n")]}</pre>
      </body>
      </xml>

fun isgraphs () : transaction page =
    return <xml>
      <body>
	<pre>{[isgraph #"a"]}</pre>
	<pre>{[isgraph (strsub "à" 0)]}</pre>
	<pre>{[isgraph #"A"]}</pre>
	<pre>{[isgraph (strsub "À" 0)]}</pre>
	<pre>{[isgraph #"1"]}</pre>
	<pre>{[isgraph #"!"]}</pre>
	<pre>{[isgraph #"#"]}</pre>
	<pre>{[not (isgraph #" ")]}</pre>
	<pre>{[not (isgraph #"\t")]}</pre>
	<pre>{[not (isdigit #"\n")]}</pre>
      </body>
      </xml>

fun islowers () : transaction page =
    return <xml>
      <body>
	<pre>{[islower #"a"]}</pre>
	<pre>{[islower (strsub "à" 0)]}</pre>
	<pre>{[not (islower #"A")]}</pre>
	<pre>{[not (islower (strsub "À" 0))]}</pre>
	<pre>{[not (islower #"1")]}</pre>
	<pre>{[not (islower #"!")]}</pre>
	<pre>{[not (islower #"#")]}</pre>
	<pre>{[not (islower #" ")]}</pre>
	<pre>{[not (islower #"\t")]}</pre>
	<pre>{[not (islower #"\n")]}</pre>
      </body>
      </xml>

fun isprints () : transaction page =
    return <xml>
      <body>
	<pre>{[isprint #"a"]}</pre>
	<pre>{[isprint (strsub "à" 0)]}</pre>
	<pre>{[isprint #"A"]}</pre>
	<pre>{[isprint (strsub "À" 0)]}</pre>
	<pre>{[isprint #"1"]}</pre>
	<pre>{[isprint #"!"]}</pre>
	<pre>{[isprint #"#"]}</pre>
	<pre>{[isprint #" "]}</pre>
	<pre>{[not (isprint #"\t")]}</pre>
	<pre>{[not (isprint #"\n")]}</pre>
      </body>
      </xml>

fun ispuncts () : transaction page =
    return <xml>
      <body>
	<pre>{[not (ispunct #"a")]}</pre>
	<pre>{[not (ispunct (strsub "à" 0))]}</pre>
	<pre>{[not (ispunct #"A")]}</pre>
	<pre>{[not (ispunct (strsub "À" 0))]}</pre>
	<pre>{[not (ispunct #"1")]}</pre>
	<pre>{[ispunct #"!"]}</pre>
	<pre>{[ispunct #"#"]}</pre>
	<pre>{[not (ispunct #" ")]}</pre>
	<pre>{[not (isprint #"\t")]}</pre>
	<pre>{[not (isprint #"\n")]}</pre>
      </body>
      </xml>

fun isspaces () : transaction page =
    return <xml>
      <body>
	<pre>{[not (isspace #"a")]}</pre>
	<pre>{[not (isspace (strsub "à" 0))]}</pre>
	<pre>{[not (isspace #"A")]}</pre>
	<pre>{[not (isspace (strsub "À" 0))]}</pre>
	<pre>{[not (isspace #"1")]}</pre>
	<pre>{[not (isspace #"!")]}</pre>
	<pre>{[not (isspace #"#")]}</pre>
	<pre>{[isspace #" "]}</pre>
	<pre>{[isspace #"\t"]}</pre>
	<pre>{[isspace #"\n"]}</pre>
      </body>
    </xml>
    
fun isuppers () : transaction page =
    return <xml>
      <body>
	<pre>{[not (isupper #"a")]}</pre>
	<pre>{[not (isupper (strsub "à" 0))]}</pre>
	<pre>{[isupper #"A"]}</pre>
	<pre>{[isupper (strsub "À" 0)]}</pre>
	<pre>{[not (isupper #"1")]}</pre>
	<pre>{[not (isupper #"!")]}</pre>
	<pre>{[not (isupper #"#")]}</pre>
	<pre>{[not (isupper #" ")]}</pre>
	<pre>{[not (isupper #"\t")]}</pre>
	<pre>{[not (isupper #"\n")]}</pre>
      </body>
      </xml>

fun isxdigits () : transaction page =
    return <xml>
      <body>
	<pre>{[isxdigit #"a"]}</pre>
	<pre>{[not (isxdigit (strsub "à" 0))]}</pre>
	<pre>{[isxdigit #"A"]}</pre>
	<pre>{[not (isxdigit (strsub "À" 0))]}</pre>
	<pre>{[isxdigit #"1"]}</pre>
	<pre>{[not (isxdigit #"!")]}</pre>
	<pre>{[not (isxdigit #"#")]}</pre>
	<pre>{[not (isxdigit #" ")]}</pre>
	<pre>{[not (isxdigit #"\t")]}</pre>
	<pre>{[not (isxdigit #"\n")]}</pre>
      </body>
      </xml>

fun tolowers () : transaction page =
    return <xml>
      <body>
	<pre>{[tolower #"A" = #"a"]}</pre>
	<pre>{[tolower #"a" = #"a"]}</pre>
	<pre>{[tolower (strsub "á" 0) = (strsub "á" 0)]}</pre>
	<pre>{[tolower (strsub "Á" 0) = (strsub "á" 0)]}</pre>
	<pre>{[tolower #"1" = #"1"]}</pre>
      </body>
    </xml>
    
fun touppers () : transaction page =
    return <xml>
      <body>
	<pre>{[toupper #"A" = #"A"]}</pre>
	<pre>{[toupper #"a" = #"A"]}</pre>
	<pre>{[toupper (strsub "á" 0) = (strsub "Á" 0)]}</pre>
	<pre>{[toupper (strsub "Á" 0) = (strsub "Á" 0)]}</pre>
	<pre>{[toupper #"1" = #"1"]}</pre>
      </body>
      </xml>

fun ord_and_chrs () : transaction page =
    return <xml>
      <body>
	<pre>{[chr (ord #"A") = #"A"]}</pre>
	<pre>{[chr (ord #"a") = #"a"]}</pre>
	<pre>{[chr (ord (strsub "á" 0)) = (strsub "á" 0)]}</pre>
	<pre>{[chr (ord (strsub "Á" 0)) = (strsub "Á" 0)]}</pre>
	<pre>{[chr (ord #"1") = #"1"]}</pre>
	<pre>{[chr (ord #"\n") = #"\n"]}</pre>
	<pre>{[chr (ord (strsub "が" 0)) = (strsub "が" 0)]}</pre>
	<pre>{[chr (ord (strsub "漢" 0)) = (strsub "漢" 0)]}</pre>
	<pre>{[chr (ord (strsub "カ" 0)) = (strsub "カ" 0)]}</pre>
      </body>
      </xml>

table t : { Id : int, Text : string }


fun test_db () : transaction page =
    dml (INSERT INTO t (Id, Text) VALUES({[1]}, {["abc"]}));
    t1 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 1);

    dml (INSERT INTO t (Id, Text) VALUES({[2]}, {["çãó"]}));
    t2 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 2);

    dml (INSERT INTO t (Id, Text) VALUES({[3]}, {["が"]}));
    t3 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 3);

    dml (INSERT INTO t (Id, Text) VALUES({[4]}, {["漢"]}));
    t4 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 4);

    dml (INSERT INTO t (Id, Text) VALUES({[5]}, {["カ"]}));
    t5 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 5);

    dml (INSERT INTO t (Id, Text) VALUES({[6]}, {["وظيفية"]}));
    t6 <- oneRow (SELECT t.Text FROM t WHERE t.Id = 6);

    return <xml>
      <body>
	<pre>{[t1.T.Text]}</pre>
	<pre>{[strlen t1.T.Text]}</pre>
	<pre>{[t2.T.Text]}</pre>
	<pre>{[strlen t2.T.Text]}</pre>
	<pre>{[t3.T.Text]}</pre>
	<pre>{[strlen t3.T.Text]}</pre>
	<pre>{[t4.T.Text]}</pre>
	<pre>{[strlen t4.T.Text]}</pre>
	<pre>{[t5.T.Text]}</pre>
	<pre>{[strlen t5.T.Text]}</pre>
	<pre>{[t6.T.Text]}</pre>
	<pre>{[strlen t6.T.Text]}</pre>
      </body>
      </xml>
