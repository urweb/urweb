fun handler f = return <xml>{[f.F1]} {[f.F2]} {[f.F3]}</xml>

val main = return <xml><body><form>
  <textbox{#F1}/>
  <textarea{#F2}/>
  <checkbox{#F3}/>
  <upload{#File}/>
  <submit action={handler}/>
</form></body></xml>
