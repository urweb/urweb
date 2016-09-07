signature FILE_IO = sig

    (* When was a source file last modified (excluding files produced after [getResetTime])? *)
    val mostRecentModTime : unit -> Time.time

    val txtOpenIn : string -> TextIO.instream
    val binOpenIn : string -> BinIO.instream

end
