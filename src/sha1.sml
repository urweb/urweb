
(* RFC-3174 (SHA-1) hashing function.
   By Tom 7, 2004: Code placed in the public domain.
*)

structure SHA1 :> SHA1 =
struct
  exception Unimplemented

  val xorb = Word32.xorb
  val andb = Word32.andb
  val orb  = Word32.orb
  val <<   = Word32.<<
  val >>   = Word32.>>
  val notb = Word32.notb
  val ++   = Word32.+

  type w32 = word
  infix xorb andb orb << >> ++

  (* workaround for andb bug in MLton 20010706 *)
  fun mkbyte w = Word32.mod (w, 0w256)

  fun ROL(X, N : Word.word) = (X << N) orb (X >> (0w32-N))

  fun wc hi lo = (hi << 0w16) orb lo

  fun w2b w = map chr
    [Word32.toInt (mkbyte (w >> 0w24)),
     Word32.toInt (mkbyte (w >> 0w16)),
     Word32.toInt (mkbyte (w >> 0w8)),
     Word32.toInt (mkbyte w)]

  (* the length (arg in bytes, output in bits)
     as a 64-bit quantity, big-endian *)
  fun lenbits l =
    implode (List.tabulate (4, fn _ => chr 0)) ^
    implode (w2b (Word32.fromInt (l * 8)))


  (* executes f for each index lo..hi-1 inclusive *)
  fun for lo hi f =
      if lo >= hi then ()
      else (ignore (f lo); for (lo + 1) hi f)

  fun ford lo hi b f =
      if lo >= hi then b
      else
        let
          val b = f (lo, b)
        in
          (ford (lo + 1) hi b f)
        end

  fun doblock (aa, bb, cc, dd, ee) msg =
    let
      val K0 = wc 0wx5A82 0wx7999
      val K1 = wc 0wx6ED9 0wxEBA1
      val K2 = wc 0wx8F1B 0wxBCDC
      val K3 = wc 0wxCA62 0wxC1D6

      fun mb n = Word32.fromInt (ord (CharVector.sub(msg, n)))

      val W = Array.array(80, 0w0)
      fun Ws x = Array.sub(W, x)

      val _ =
        for 0 16
        (fn t =>
         let in
           Array.update(W, t,
                        (mb (t * 4    ) << 0w24) orb
                        (mb (t * 4 + 1) << 0w16) orb
                        (mb (t * 4 + 2) << 0w8)  orb
                        (mb (t * 4 + 3)))
         end)

      val _ =
        for 16 80
        (fn t =>
         let
           val n =
             Ws (t-3)  xorb
             Ws (t-8)  xorb
             Ws (t-14) xorb
             Ws (t-16)
           val zz = ROL(n, 0w1)
         in
           Array.update(W, t, zz)
         end)


      val (A, B, C, D, E) = (aa, bb, cc, dd, ee)


      fun round lo hi f k ctxt =
        ford lo hi ctxt
        (fn (t, ctxt as (A, B, C, D, E)) =>
         let
           val temp = ROL(A, 0w5) ++ (f ctxt) ++ E ++ Ws t ++ k
           val E = D;
           val D = C;
           val C = ROL(B, 0w30)
           val B = A
           val A = temp
         in
           (A, B, C, D, E)
         end)

      val (A, B, C, D, E) =
        round 0 20 (fn (A, B, C, D, E) =>
                    ((B andb C) orb ((notb B) andb D)))
                   K0 (A, B, C, D, E)

      val (A, B, C, D, E) =
        round 20 40 (fn (A, B, C, D, E) =>
                     (B xorb C xorb D))
                    K1 (A, B, C, D, E)

      val (A, B, C, D, E) =
        round 40 60 (fn (A, B, C, D, E) =>
                     ((B andb C) orb (B andb D) orb (C andb D)))
                    K2 (A, B, C, D, E)

      val (A, B, C, D, E) =
        round 60 80 (fn (A, B, C, D, E) =>
                     (B xorb C xorb D))
                    K3 (A, B, C, D, E)

    in
      (aa ++ A, bb ++ B, cc ++ C, dd ++ D, ee ++ E)
    end

  datatype 'a stream =
      Cons of ('a * (unit -> 'a stream))
    | Nil

  (* turn a stream of oddly chunked strings into
     one with 512-bit blocks *)
  fun chunk_512 s =
    let

      (* the padding required to make a message of length l (bytes)
         a proper SHA-1 input. Returns either one or two Cons cells.
         tail is the end of the input (63 bytes or less)
         l is the total length of the input, *including* the length of the
         tail end *)
      fun padding tail l =
          let val v = l mod 64 in
            if v < 56 then
              let val p = 56 - v
                val padding = implode (List.tabulate (p - 1, fn _ => chr 0))
              in Cons (tail ^ str (chr 0x80) ^ padding ^ lenbits l,
                    fn _ => Nil)
              end
            else if v < 64 then
              let val p = 64 - v
                val padding1 = implode (List.tabulate (p - 1, fn _ => chr 0))
                val padding2 = implode (List.tabulate (56, fn _ => chr 0))
              in Cons (tail ^ str (chr 0x80) ^ padding1,
                    fn _ => Cons (padding2 ^ lenbits l, fn _ => Nil))
              end
            else raise Unimplemented (* Impossible? *)
          end

      (* n is the bytes we've already output.
         cur is a string (of 64 bytes or less) that will
         be our next chunk.
         rest,sofar is a string and index indicating the
         next bit of data. *)
      (* PERF Could be more efficient by using an
         accumulating array instead of a string for cur *)
      fun ch n cur sofar startat () =
        (* if we already have 64 bytes, return it *)
        if size cur = 64
        then
          let in
            Cons(cur, ch (n + 64) "" sofar startat)
          end
        else
          (* do we have any in 'sofar'? *)
          if startat < size sofar
          then let
                 val get = Int.min(size sofar - startat,
                                   64 - size cur)
               in
                 (* be eager, since we need to return something now *)
                 ch n (cur ^ String.substring(sofar, startat, get))
                    sofar (startat + get) ()
               end
          else
            (* sofar has been exhausted,
               so get some from input stream *)
            (case s () of
               (* eager, again *)
               SOME ss => ch n cur ss 0 ()
             | NONE =>
                 (* no more data. *)
                 padding cur (n + size cur))
    in
      ch 0 "" "" 0
    end

  fun hash_stream orig_stream =
    let

      val stream512 = chunk_512 orig_stream

      (* gets hash context, length of string so far (bytes),
         and tail of stream *)
      fun hash_rest stream ctxt =
        (case stream() of
           Cons (s, stream) =>
             let val ctxt = doblock ctxt s
             in  hash_rest stream ctxt
             end
         | Nil => ctxt)

      val init =
        (wc 0wx6745 0wx2301,
         wc 0wxefcd 0wxab89,
         wc 0wx98ba 0wxdcfe,
         wc 0wx1032 0wx5476,
         wc 0wxc3d2 0wxe1f0)

      val (a, b, c, d, e) = hash_rest stream512 init
    in
      implode (w2b a @ w2b b @ w2b c @ w2b d @ w2b e)
    end

  fun hash m =
    hash_stream
    (let val r = ref true
     in (fn () =>
         if !r
         then (r := false; SOME m)
         else NONE)
     end)

  val digits = "0123456789ABCDEF"
  fun bintohex s =
    String.translate (fn c =>
                      implode [CharVector.sub (digits, ord c div 16),
                               CharVector.sub (digits, ord c mod 16)]) s

  (* ASCII trick: (ch | 4400) % 55 *)
  fun hexvalue ch =
    SysWord.toInt (SysWord.orb(SysWord.fromInt(ord ch), SysWord.fromInt 4400)) mod 55

  fun parse_hex s =
      if size s <> 40
         orelse not (CharVector.all (fn c => (ord c >= ord #"0" andalso
                                              ord c <= ord #"9") orelse
                                     (ord c >= ord #"a" andalso
                                      ord c <= ord #"f") orelse
                                     (ord c >= ord #"A" andalso
                                      ord c <= ord #"F")) s)
      then NONE
      else SOME (CharVector.tabulate(20,
                                     (fn i =>
                                      chr(hexvalue (String.sub(s, i * 2)) * 16 +
                                          hexvalue (String.sub(s, i * 2 + 1))))))

end
