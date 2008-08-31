val handler = fn x => <html><body>
        You entered: {cdata x.A}
</body></html>

val main = fn () => <html><body>
        <lform>
                <radio{#A}>
                        <li> <radioOption value="A"/>A</li>
                        <li> <radioOption value="B"/>B</li>
                </radio>
                <submit action={handler}/>
        </lform>
</body></html>
