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
  val readLiteral : int Tree.t -> BitIO.instream -> int
end = struct
  type buffer = Word8Vector.vector list
  type instream = { buf : buffer ref,
                    bitins : BitIO.instream,
                    prev : Word8RingBuffer.t,
                    exhausted : bool ref }

  fun fromBitInstream bitins : instream =
        { buf = ref [],
          bitins = bitins,
          (* the LZ77 algorithm may use a reference to a duplicated string
             occurring in a previous block, up to 32K input bytes before. *)
          prev = Word8RingBuffer.create 32768,
          exhausted = ref false }

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
  fun readStored bitins =
        let
          fun unpackInt vec =
                Word8Vector.foldr
                  (fn (byte, int) => int * 0x100 + Word8.toInt byte)
                  0
                  vec
          (* using inputN effectively skips remaining bits *)
          val len = BitIO.inputN (bitins, 2)
          val nlen = BitIO.inputN (bitins, 2)
          val _ = nlen = invert len orelse raise Fail "didn't match complement"
          val v = BitIO.inputN (bitins, unpackInt len)
        in
          v
        end

  (* decode literal/length value from input stream *)
  fun readLiteral (Tree.Leaf value) _ = value
    | readLiteral (Tree.Node (zero, one)) bitins =
        let
          val bit = BitIO.bits (bitins, 0w1)
        in
          if bit = 0w0 then readLiteral zero bitins
          else readLiteral one bitins
        end

  local
    val length = Vector.fromList
      [  3,   4,   5,   6,   7,   8,   9,  10,  11, 13,
        15,  17,  19,  23,  27,  31,  35,  43,  51, 59,
        67,  83,  99, 115, 131, 163, 195, 227, 258]
    val extraBits = Vector.fromList
      [0w0, 0w0, 0w0, 0w0, 0w0, 0w0, 0w0, 0w0, 0w1,0w1,
       0w1, 0w1, 0w2, 0w2, 0w2, 0w2, 0w3, 0w3, 0w3,0w3,
       0w4, 0w4, 0w4, 0w4, 0w5, 0w5, 0w5, 0w5, 0w0]
  in
    fun decodeLength (alphabet, bitins) =
          let
            val index = alphabet - 257
            val length = Vector.sub (length, index)
            val extraBits = Vector.sub (extraBits, index)
            val extra = BitIO.bits (bitins, extraBits)
          in
            length + (Word.toInt extra)
          end
  end

  local
    val dist = Vector.fromList
      [   1,    2,    3,    4,    5,    7,     9,    13,    17,   25,
         33,   49,   65,   97,  129,  193,   257,   385,   513,  769,
       1025, 1537, 2049, 3073, 4097, 6145,  8193, 12289, 16385, 24577]
    val extraBits = Vector.fromList
      [ 0w0,  0w0,  0w0,  0w0,  0w1,  0w1,   0w2,   0w2,   0w3,  0w3,
        0w4,  0w4,  0w5,  0w5,  0w6,  0w6,   0w7,   0w7,   0w8,  0w8,
        0w9,  0w9, 0w10, 0w10, 0w11, 0w11,  0w12,  0w12,  0w13, 0w13]
  in
    fun decodeDistance bitins =
          let
            val code = Word.toInt (BitIO.bits (bitins, 0w5))
            val dist = Vector.sub (dist, code)
            val extraBits = Vector.sub (extraBits, code)
            val extra = BitIO.bits (bitins, extraBits)
          in
            dist + (Word.toInt extra)
          end
  end

  fun readCompressed huffman (ins as {bitins, prev, ...}) =
        let
          val segmentSize = 256
          val segment = Word8Buffer.create segmentSize
          fun put (segment, value) = (
                Word8Buffer.putElem (segment, value);
                Word8RingBuffer.putElem (prev, value))
          fun read segments =
                let
                  val value = readLiteral huffman bitins
                in
                  if value < 0x100 then (
                    put (segment, Word8.fromInt value);
                    if Word8Buffer.isFull segment then
                      read (Word8Buffer.flush segment::segments)
                    else
                      read segments)
                  else if value = 0x100 then
                    Word8Vector.concat (rev (Word8Buffer.flush segment::segments))
                  else (
                    let
                      val segments' = Word8Buffer.flush segment::segments
                      val length = decodeLength (value, bitins)
                      val dist = decodeDistance bitins
                      val segment = Word8Buffer.create length
                      fun copy () =
                            if Word8Buffer.isFull segment then ()
                            else
                              let
                                val byte = Word8RingBuffer.getElem (prev, dist)
                              in
                                put (segment, byte);
                                copy ()
                              end
                    in
                      copy ();
                      read (Word8Buffer.freeze segment::segments')
                    end)
                end
        in
          read []
        end

  fun readTree (ins as {bitins, prev, ...}) =
        raise Fail "unimplemented"

  (* 3.2.3. Details of block format *)
  fun extend (ins as {buf, bitins, exhausted, ...} : instream) =
        let
          (* first bit       BFINAL *)
          val bfinal = BitIO.bits (bitins, 0w1)
          (* next 2 bits     BTYPE *)
          val btype = BitIO.bits (bitins, 0w2)
        in
          case btype of
               (* 00 - no compression *)
               0w0 => buf := !buf @ [readStored bitins]
               (* 01 - compressed with fixed Huffman codes *)
             | 0w1 => buf := !buf @ [readCompressed fixed ins]
               (* 10 - compressed with dynamic Huffman codes *)
             | 0w2 =>
                 let
                   val tree = readTree ins
                 in
                   buf := !buf @ [readCompressed tree ins]
                 end
               (* 11 - reserved (error) *)
             | _ => raise Fail "invalid block type";
          if bfinal = 0w0 then () else exhausted := true
        end

  fun input {buf as ref (v::vs), ...} = (buf := vs; v)
    | input {buf as ref [], exhausted as ref true, ...} = raise Fail "exausted"
    | input (ins : instream) = (extend ins; input ins)

  fun endOfStream ({buf as ref (_::_), ...} : instream) = false
    | endOfStream {buf as ref [], exhausted as ref true, ...} = true
    | endOfStream {buf as ref [], bitins, ...} = BitIO.endOfStream bitins

end
