(* RFC 1951 *)
structure Deflate :> sig
  type instream
  val fromBitInstream : BitIO.instream -> instream
  val fromBinInstream : BinIO.instream -> instream
  val fromBytes : Word8Vector.vector -> instream
  val input : instream -> Word8Vector.vector
  val endOfStream : instream -> bool
  val construct : int array -> int Tree.t
  val fixed : int Tree.t
  val readLiteral : int Tree.t -> instream -> int
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

  fun incrArrayElem (arr, i) =
        let
          val old = Array.sub (arr, i)
          val new = old + 1
        in
          Array.update (arr, i, new)
        end

  fun max (a, b) = if a > b then a else b

  (* 3.2.2. Use of Huffman coding in the "deflate" format *)
  fun construct lengths =
        let
          val numAlphabets = Array.length lengths
          val maxAlphabet = numAlphabets - 1
          val maxBits = Array.foldr max 0 lengths
          (* 1)  Count the number of codes for each code length.  Let
                 bl_count[N] be the number of codes of length N, N >= 1. *)
          val blCount (* length -> count *) = Array.array (maxBits + 1, 0)
          fun makeBlCount alphabet =
                if alphabet > maxAlphabet then ()
                else (
                  incrArrayElem (blCount, Array.sub (lengths, alphabet));
                  makeBlCount (alphabet + 1))
          val _ = makeBlCount 0
          (* 2)  Find the numerical value of the smallest code for each
                 code length *)
          val nextCode (* length -> code *) = Array.array (maxBits + 1, 0)
          fun makeNextCode bits =
                if bits >= maxBits then ()
                else
                  let
                    (* the smallest code for this length *)
                    val this = Array.sub (nextCode, bits)
                    (* the number of codes of this length *)
                    val count = Array.sub (blCount, bits)
                    (* the smallest code for next length *)
                    val next = (this + count) * 2
                  in
                    Array.update (nextCode, bits + 1, next);
                    makeNextCode (bits + 1)
                  end
          val _ = makeNextCode 0
          (* 3)  Assign numerical values to all codes, using consecutive
                 values for all codes of the same length with the base
                 values determined at step 2. Codes that are never used
                 (which have a bit length of zero) must not be assigned a
                 value. *)
          val codes (* alphabet -> code *) = Array.array (numAlphabets, 0)
          fun makeCode alphabet =
                if alphabet > maxAlphabet then ()
                else
                  let
                    val len = Array.sub (lengths, alphabet)
                    val code = Array.sub (nextCode, len)
                  in
                    if len <> 0 then (
                      Array.update (codes, alphabet, code);
                      incrArrayElem (nextCode, len))
                    else ();
                    makeCode (alphabet + 1)
                  end
          val _ = makeCode 0
          val paths =
            Array.foldri
              (fn (alphabet, length, b) =>
                let
                  val code = Word.fromInt (Array.sub (codes, alphabet))
                  val edges = Edge.fromWordAndLength (code, length)
                in
                  (edges, alphabet)::b
                end)
              []
              lengths
        in
          Tree.make paths
        end

  (* 3.2.6. Compression with fixed Huffman codes (BTYPE=01) *)
  val fixed =
        let
          fun f code =
            if code < 144 then 8
            else if code < 256 then 9
            else if code < 280 then 7
            else if code < 288 then 8
            else raise Fail "undefined code"
        in
          construct (Array.tabulate (288, f))
        end

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

  (* decode literal/length value from input stream *)
  fun readLiteral (Tree.Leaf value) _ = value
    | readLiteral (Tree.Node (zero, one)) (ins as {buf as ref vs, bitins}) =
        let
          val bit = BitIO.bits (bitins, 0w1)
        in
          if bit = 0w0 then readLiteral zero ins
          else readLiteral one ins
        end

  fun readCompressed huffman (ins as {buf as ref vs, bitins}) =
        let
          val value = readLiteral huffman ins
        in
          if value < 0x100 then
            buf := vs @ [Word8Vector.tabulate (1, fn i => Word8.fromInt value)];
          else if value = 0x100 then
            raise Fail "unimplemented 0x100"
          else
            raise Fail "unimplemented >0x100"
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
             | 0w1 =>
                 readCompressed fixed ins
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
