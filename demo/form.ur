fun handler r = return <xml><body>
  <table>
    <tr> <th>A:</th> <td>{[r.A]}</td> </tr>
    <tr> <th>B:</th> <td>{[r.B]}</td> </tr>
    <tr> <th>C:</th> <td>{[r.C]}</td> </tr>
  </table>
</body></xml>

fun main () = return <xml><body>
  <form>
    <table>
      <tr> <th>A:</th> <td><textbox{#A}/></td> </tr>
      <tr> <th>B:</th> <td><textbox{#B}/></td> </tr>
      <tr> <th>C:</th> <td><checkbox{#C}/></td> </tr>
      <tr> <th/> <td><submit action={handler}/></td> </tr>
    </table>
  </form>
</body></xml>
