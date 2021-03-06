structure BinRandomAccessFile :> sig
  type infile
  val openIn : string -> infile
  val read : infile * int -> Word8Vector.vector
  val tellIn : infile -> Position.int
  val seekIn : infile * Position.int -> unit
  val endPosIn : infile -> Position.int
  val closeIn : infile -> unit
end = struct
  type infile = BinIO.StreamIO.reader

  fun openIn fileName =
        let
          val ins = BinIO.getInstream (BinIO.openIn fileName)
          val (reader, buffer) = BinIO.StreamIO.getReader ins
        in
          reader
        end

  fun read (BinPrimIO.RD {readVec = SOME readVec, ...}, n) =
        readVec n
    | read _ = raise Fail "BinRandomAccessFile.read not supported"

  fun tellIn (BinPrimIO.RD {getPos = SOME getPos, ...}) =
        getPos ()
    | tellIn _ = raise Fail "BinRandomAccessFile.tellIn not supported"

  fun seekIn (BinPrimIO.RD {setPos = SOME setPos, ...}, pos) =
        setPos pos
    | seekIn _ = raise Fail "BinRandomAccessFile.seekIn not supported"

  fun endPosIn (BinPrimIO.RD {endPos = SOME endPos, ...}) =
        endPos ()
    | endPosIn _ = raise Fail "BinRandomAccessFile.lengthIn not supported"

  fun closeIn (BinPrimIO.RD {close, ...}) = close ()
end
