fun doSubmit r =
  return <xml>Done {[readError r.Amount1 * readError r.Amount2 * 2.0]}</xml>

fun main () =
  amount1S <- source "1";
  amount2S <- source "1";
  return <xml>  <body>
   <form>
      <table>
        <tr><td>Amount1:</td><td><textbox{#Amount1}
source={amount1S}/></td></tr>
        <tr><td>Amount2:</td><td><textbox{#Amount2}
source={amount2S}/></td></tr>
        <tr><td>Total:</td><td><dyn signal={showTotal amount1S
amount2S}/></td></tr>
      </table>
      <submit value="Buy" action={doSubmit}/>
    </form>
  </body>
</xml>

and showTotal amount1S amount2S =
  a1 <- signal amount1S;
  a2 <- signal amount2S;
  return (case ((read a1), (read a2)) of
           (None, _) => <xml></xml>
         | (_, None) => <xml></xml>
         | (Some a, Some b) => <xml>{[a * b * 2.0]}</xml>)
