(* RFC 1951 *)
structure Deflate :> sig
  type instream
  val fromBitInstream : BitIO.instream -> instream
  val fromBinInstream : BinIO.instream -> instream
  val fromBytes : Word8Vector.vector -> instream
  val input : instream -> Word8Vector.vector
  val endOfStream : instream -> bool
end = struct
  fun unpackInt vec =
    Word8Vector.foldr (fn (byte, int) => int * 0x100 + Word8.toInt byte) 0 vec

  type buffer = Word8Vector.vector list
  type instream = { buf : buffer ref,
                    bitins : BitIO.instream }

  fun fromBitInstream bitins : instream = {buf = ref [], bitins = bitins}

  val fromBinInstream = fromBitInstream o BitIO.fromBinInstream
  val fromBytes = fromBitInstream o BitIO.fromBytes

  val invert = Word8Vector.map Word8.notb

  (* 3.2.4. Non-compressed blocks (BTYPE=00) *)
  fun readStored {buf as ref vs, bitins} =
        let
          (* using inputN effectively skips remaining bits *)
          val len = BitIO.inputN (bitins, 2)
          val nlen = BitIO.inputN (bitins, 2)
          val _ = nlen = invert len orelse raise Fail "didn't match complement"
          val v = BitIO.inputN (bitins, unpackInt len)
        in
          buf := vs @ [v]
        end

  (* 3.2.3. Details of block format *)
  fun extend (ins as {buf, bitins}) =
        let
          (* first bit       BFINAL *)
          val bfinal = BitIO.bits (bitins, 0w1)
          (* next 2 bits     BTYPE *)
          val btype = BitIO.bits (bitins, 0w1)
        in
          case btype of
               (* 00 - no compression *)
               0w0 => readStored ins
               (* 01 - compressed with fixed Huffman codes *)
             | 0w1 => raise Fail "unimplemented"
               (* 10 - compressed with dynamic Huffman codes *)
             | 0w2 => raise Fail "unimplemented"
               (* 11 - reserved (error) *)
             | _ => raise Fail "invalid block type"
        end

  fun input {buf as ref (v::vs), ...} = (buf := vs; v)
    | input (ins : instream) = (extend ins; input ins)

  fun endOfStream {buf as ref (_::_), ...} = false
    | endOfStream {buf as ref [], bitins} = BitIO.endOfStream bitins

end
