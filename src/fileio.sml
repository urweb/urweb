structure FileIO :> FILE_IO = struct

val mostRecentModTimeRef = ref (Time.zeroTime)

fun checkFileModTime fname =
  let val mtime = OS.FileSys.modTime fname in
      if Time.compare (mtime, !mostRecentModTimeRef) = GREATER andalso
         Time.compare (mtime, Globals.getResetTime ()) = LESS
      then mostRecentModTimeRef := mtime
      else ()
  end

fun mostRecentModTime () =
  if Time.compare (!mostRecentModTimeRef, Time.zeroTime) = EQUAL
  then Globals.getResetTime ()
  else !mostRecentModTimeRef

fun txtOpenIn fname =
  let
      val inf = TextIO.openIn fname
      val () = checkFileModTime fname
  in
      inf
  end

fun binOpenIn fname =
  let
      val inf = BinIO.openIn fname
      val () = checkFileModTime fname
  in
      inf
  end

end
