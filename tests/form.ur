val handler = fn r => <html><body>
        <li> Name: {cdata r.Nam}</li>
        <li> Word: {cdata r.Word}</li>
</body></html>

val main : unit -> page = fn () => <html><body>
        <lform>
                Name: <textbox{#Nam} /><br/>
                Word: <textbox{#Word} /><br/>

                <submit action={handler}/>
        </lform>
</body></html>
