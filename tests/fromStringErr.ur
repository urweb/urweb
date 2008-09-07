fun main () : transaction page = return <html><body>
        3 = {cdata (show _ (readError _ "3" : int))}<br/>
        12.12 = {cdata (show _ (readError _ "12.12" : float))}<br/>
        Hi = {cdata (show _ (readError _ "Hi" : string))}<br/>
        True = {cdata (show _ (readError _ "True" : bool))}<br/>
</body></html>
