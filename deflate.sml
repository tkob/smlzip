structure Defalte :> sig
  type instream
  val fromBitInstream : BitIO.instream -> instream
  val fromBinInstream : BinIO.instream -> instream
  val fromBytes : Word8Vector.vector -> instream
  val input : instream -> Word8Vector.vector
end = struct
  fun unpackInt vec =
    Word8Vector.foldr (fn (byte, int) => int * 0x100 + Word8.toInt byte) 0 vec

  type buffer = Word8Vector.vector list
  type instream = buffer ref * BitIO.instream

  fun fromBitInstream bitins : instream = (ref [], bitins)

  val fromBinInstream = fromBitInstream o BitIO.fromBinInstream
  val fromBytes = fromBitInstream o BitIO.fromBytes

  fun readStored (buf as ref vs, bitins) =
  let
    val len = BitIO.inputN (bitins, 2)
    val nlen = BitIO.inputN (bitins, 2)
    val v = BitIO.inputN (bitins, unpackInt len)
  in
    buf := vs @ [v]
  end

  fun extend (ins as (buf as ref (v::vs), bitins)) =
  let
    val bfinal = BitIO.bits (bitins, 0w1)
    val btype = BitIO.bits (bitins, 0w1)
  in
    case btype of
         0w0 => readStored ins
       | 0w1 => raise Fail "unimplemented"
       | 0w2 => raise Fail "unimplemented"
       | 0w3 => raise Fail "invalid block type"
  end

  fun input (buf as ref (v::vs), _) = (buf := vs; v)
    | input (ins : instream) = (extend ins; input ins)

end
