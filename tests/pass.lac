val handler = fn r => <html><body>
        <li> Name: {cdata r.Nam}</li>
        <li> Password: {cdata r.Word}</li>
</body></html>

val main : unit -> page = fn () => <html><body>
        <lform>
                Name: <textbox{#Nam} /><br/>
                Password: <password{#Word} /><br/>

                <submit action={handler}/>
        </lform>
</body></html>
