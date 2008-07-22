val handler1 = fn r => <html><body>
        <li> Name: {cdata r.Nam}</li>
        <li> Word: {cdata r.Word}</li>
</body></html>

val handler2 = fn r => <html><body>
        <li> Name: {cdata r.Nam}</li>
        <li> Ward: {cdata r.Ward}</li>
</body></html>

val main : unit -> page = fn () => <html><body>
        <lform>
                Name: <textbox{#Nam} /><br/>
                Word: <textbox{#Word} /><br/>

                <submit action={handler1}/>
        </lform>

        <lform>
                Name: <textbox{#Nam} /><br/>
                Word: <textbox{#Ward} /><br/>

                <submit action={handler2}/>
        </lform>
</body></html>
