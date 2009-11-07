open Meta

val title = string "Title"
val abstract = textarea "Abstract"
val commentsForAuthors = textarea "Comments for Authors"

fun charIn s =
    if String.length s = 0 then
        error <xml>Impossible: Empty option value</xml>
    else
        String.sub s 0

con dropdown = (char, string)
fun dropdown name opts = {Nam = name,
                          Show = txt,
                          Widget = fn [nm :: Name] => <xml><select{nm}>
                            {List.mapX (fn x => <xml><option>{[x]}</option></xml>) opts}
                          </select></xml>,
                          WidgetPopulated = fn [nm :: Name] v => <xml><select{nm}>
                            {List.mapX (fn x => <xml><option selected={x = v}>{[x]}</option></xml>) opts}
                          </select></xml>,
                          Parse = charIn,
                          Inject = _}

val dropdown_show = _
