(* imperative bit stream *)
structure BitIO :> sig
  type instream
  val fromBinInstream : BinIO.instream -> instream
  val fromBytes : Word8Vector.vector -> instream
  val bits : instream * Word.word -> Word.word
  val inputN : instream * int -> Word8Vector.vector
end = struct

  type fraction = { buf : Word.word, size : Word.word }
  type input1 = unit -> Word8.word option
  type inputN = int -> Word8Vector.vector
  type instream = fraction ref * input1 * inputN

  fun fromBinInstream binins =
    (ref {buf = 0w0, size = 0w0},
    fn () => BinIO.input1 binins,
    fn n => BinIO.inputN (binins, n))

  fun fromBytes bytes =
  let
    val pos = ref 0
    fun input1 () =
      if !pos >= Word8Vector.length bytes then NONE
      else
        SOME (Word8Vector.sub (bytes, !pos))
        before pos := !pos + 1
    fun inputN n = raise Fail "unimplemented"
  in
    (ref {buf = 0w0, size = 0w0}, input1, inputN)
  end

  fun bits (ins as (frac as ref {buf, size}, input1, _), need) =
  let
    val fromWord8ToWord = Word.fromLarge o Word8.toLarge
    open Word
    infix andb orb >> <<
  in
    if size >= need then
      (* consume if the buffer has enough bits *)
      (frac := {buf = buf >> need, size = size - need};
      buf andb ((0w1 << need) - 0w1)) (* bit above need bits are zeroed *)
    else
      (* otherwise, extend the buffer and retry *)
      case input1 () of
           NONE => raise Fail "Not enough bits left in the stream"
         | SOME byte =>
             (frac := {buf = (fromWord8ToWord byte << size) orb buf, size = size + 0w8};
             bits (ins, need))
  end

  fun inputN (ins as (frac, _, inputN), n) =
    (frac := {buf = 0w0, size = 0w0}; inputN n)
end
