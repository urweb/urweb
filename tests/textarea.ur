val handler = fn x => <html><body>
        You entered: {cdata x.A}
</body></html>

val main = fn () => <html><body>
        <lform>
                <ltextarea{#A}/>
                <submit action={handler}/>
        </lform>
</body></html>
