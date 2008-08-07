val handler = fn x => <html><body>
        {if x.A then cdata "Yes" else cdata "No"}
</body></html>

val main = fn () => <html><body>
        <lform>
                <checkbox{#A}/> How about it?<br/>
                <submit action={handler}/>
        </lform>
</body></html>
