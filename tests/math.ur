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
  <button value="asin of 0.1!" onclick={fn _ => alert (show (asin 0.1))}/>
  {[(asin 0.1)]}
  <button value="acos of 0.1!" onclick={fn _ => alert (show (acos 0.1))}/>
  {[(acos 0.1)]}
  <button value="atan of 0.1!" onclick={fn _ => alert (show (atan 0.1))}/>
  {[(atan 0.1)]}
  <button value="atan2 of 0.1 and -0.2!" onclick={fn _ => alert (show (atan2 0.1 (-0.2)))}/>
  {[(atan2 0.1 (-0.2))]}
  <button value="floor of 34.5!" onclick={fn _ => alert (show (floor 34.5))}/>
  {[(floor 34.5)]}
  <button value="abs of -10.0!" onclick={fn _ => alert (show (abs (-10.0)))}/>
  {[(abs (-10.0))]}
  </body></xml>
