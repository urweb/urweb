fun handler r = return <xml><body>
</body></xml>

fun main () =
    return <xml><body>
      <form>
        <textbox{#A} required placeholder="bobby"/>
        <textbox{#B} placeholder="soggy" autofocus/>

        <submit action={handler}/>
      </form>
    </body></xml>
