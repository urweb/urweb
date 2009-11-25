fun main () : transaction page = return <xml>
 <body>
   <form>
     <textbox{#Text}/><submit action={submit}/>
   </form>
 </body>
</xml>

and submit r = return <xml/>
