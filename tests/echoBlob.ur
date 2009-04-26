fun echo r = returnBlob (fileData r.Data) (blessMime (fileMimeType r.Data))

fun main () = return <xml><body>
  <form>
    <upload{#Data}/>
    <submit action={echo}/>
  </form>
</body></xml>
