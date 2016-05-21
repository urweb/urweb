fun main () = return <xml><body>
  <button value="Power 2.0 of 2.0!" onclick={fn _ => alert (show (pow 2.0 2.0))}/>
  {[(pow 2.0 2.0)]}
  <button value="Square root of 25!" onclick={fn _ => alert (show (sqrt 25.0))}/>
  {[(sqrt 25.0)]}
  <button value="Sin of 0.1!" onclick={fn _ => alert (show (sin 0.1))}/>
  {[(sin 0.1)]}
  <button value="Cos of 0.1!" onclick={fn _ => alert (show (cos 0.1))}/>
  {[(cos 0.1)]}
  <button value="log of 0.1!" onclick={fn _ => alert (show (log 0.1))}/>
  {[(log 0.1)]}
  <button value="Exp of 0.1!" onclick={fn _ => alert (show (exp 0.1))}/>
  {[(exp 0.1)]}
  </body></xml>
