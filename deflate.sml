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

  fun incrArrayElem (arr, i) =
        let
          val old = Array.sub (arr, i)
          val new = old + 1
        in
          Array.update (arr, i, new)
        end

  fun max (a, b) = if a > b then a else b

  (* 3.2.6. Compression with fixed Huffman codes (BTYPE=01) *)
  val lenFixed =
        let
          fun f code =
            if code < 144 then 8
            else if code < 256 then 9
            else if code < 280 then 7
            else if code < 288 then 8
            else raise Fail "undefined code"
        in
          Array.tabulate (288, f)
        end

  (* 3.2.2. Use of Huffman coding in the "deflate" format *)
  fun construct (lenf, maxbits, maxcode) =
        let
          (* 1)  Count the number of codes for each code length.  Let
                 bl_count[N] be the number of codes of length N, N >= 1. *)
          val blCount (* length -> count *) = Array.array (maxbits + 1, 0)
          fun makeBlCount code =
                if code > maxcode then ()
                else (
                  incrArrayElem (blCount, lenf code);
                  makeBlCount (code + 1))
          val _ = makeBlCount 0
        in
          blCount
        end

  fun input {buf as ref (v::vs), ...} = (buf := vs; v)
    | input (ins : instream) = (extend ins; input ins)

  fun endOfStream {buf as ref (_::_), ...} = false
    | endOfStream {buf as ref [], bitins} = BitIO.endOfStream bitins

end
