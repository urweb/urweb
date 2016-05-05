fun main () : transaction page =
    setHeader (blessResponseHeader "Content-Disposition")
              ("attachment; filename=test.txt");
    returnBlob (textBlob "Hi there!") (blessMime "text/plain")
