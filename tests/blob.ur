fun main () =
  setHeader (blessResponseHeader "X-Test") "Test";
  return <xml><body>Test</body></xml>

fun bad () =
   setHeader (blessResponseHeader "X-Test") "Test";
   returnBlob (textBlob "hello") (blessMime "text/plain")
