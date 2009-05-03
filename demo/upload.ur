fun echo r =
  if blobSize (fileData r.File) > 100000 then
      return <xml>Whoa!  That one's too big.</xml>
  else
      returnBlob (fileData r.File) (blessMime (fileMimeType r.File))

fun main () = return <xml><body>
  <h1>The Amazing File Echoer!</h1>

  <form>Upload a file: <upload{#File}/> <submit action={echo}/></form>
</body></xml>
