fun main () : transaction page = return <xml><body>
  {txt _ (1 = 1)}, {txt _ (1 = 2)}<br/>
  {txt _ (1 <> 1)}, {txt _ (1 <> 2)}<br/>
  {txt _ (True = True)}, {txt _ (True = False)}<br/>
  {txt _ (True <> True)}, {txt _ (True <> False)}<br/>
  {txt _ ("A" = "A")}, {txt _ ("A" = "B")}<br/>
  {txt _ ("A" <> "A")}, {txt _ ("A" <> "B")}<br/>
</body></xml>
