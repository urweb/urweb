fun annoyer () =
    alert "Hi!";
    sleep 5000;
    annoyer ()

fun main () : transaction page = return <xml><body onload={annoyer ()}/></xml>

