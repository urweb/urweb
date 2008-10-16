val handler = fn x => <xml><body>
        You entered: {cdata x.A}
</body></xml>

val main = fn () => <xml><body>
        <form>
                <select{#A}>
                        <option value="A">A</option>
                        <option value="B">B</option>
                </select>
                <submit action={handler}/>
        </form>
</body></xml>
