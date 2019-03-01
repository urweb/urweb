fun main () : transaction page = return <xml>
 <body>
   <form>
     <textbox{#Text}/>
     <submit action={handler}/>
   </form>
 </body>
</xml>

and handler r = return <xml/>
