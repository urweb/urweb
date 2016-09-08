
(* Implementation the SHA-1 hash function.
   Written by Tom 7 in 2004; code in the public domain. *)

signature SHA1 =
sig

  (* Perform the SHA-1 hash function on a message.
     Returns the 160 bit (20 byte) hash.

     recall that string = CharVector.vector.
     The input string may contain non-ascii data;
     the output certainly will. *)

  val hash : string -> string

  (* pass in a stream as stateful function that returns
     SOME s for some non-empty prefix of the remainder of
     the stream, or NONE when the stream has ended. *)
  val hash_stream : (unit -> string option) -> string

  (* XXX move to hashutil *)
  (* convert a binary string to one built of hex digits *)
  val bintohex : string -> string

  (* Parse a hexadecimal SHA-1 string. Uppercase and lowercase
     are permitted. If the string is not the right length or
     contains invalid characters, returns NONE. *)
  val parse_hex : string -> string option

end
