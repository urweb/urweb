fun main () =
    b <- source True;
    let
        fun textboxList xs = <xml>
          <table>
            {List.mapX (fn src => <xml><tr>
              <td dynClass={return null} dynStyle={b <- signal b;
                                                   if b then
                                                       return (STYLE "width: 500px")
                                                   else
                                                       return (STYLE "width: 100px")}>
                <ctextbox source={src}/>
            </td></tr></xml>) xs}
          </table>
        </xml>
    in
        s <- source "foo";
        return <xml><body>
          <ccheckbox source={b}/>
          {textboxList (s :: s :: [])}
        </body></xml>
    end
