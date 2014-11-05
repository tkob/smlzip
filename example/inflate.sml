(* make BinIO.instream from Posix.IO.file_desc *)
fun mkInstream fd = 
let 
  val reader = Posix.IO.mkBinReader 
      {fd = fd, name = "", initBlkMode = true} 
  val instream = BinIO.StreamIO.mkInstream 
      (reader, Word8Vector.fromList []) 
in 
  BinIO.mkInstream instream 
end 

(* make BinIO.outstream from Posix.IO.file_desc *)
fun mkOutstream fd = 
let 
  val writer = Posix.IO.mkBinWriter
      {fd = fd, name = "",
       appendMode = true,
       initBlkMode = true,
       chunkSize = 1024}
  val outstream = BinIO.StreamIO.mkOutstream
      (writer, IO.NO_BUF)
in 
  BinIO.mkOutstream outstream 
end 


fun main () =
let
  (* val ins = (Deflate.fromBinInstream o mkInstream) Posix.FileSys.stdin *)
  val fileName = List.hd (CommandLine.arguments ())
  val ins = Deflate.fromBinInstream (BinIO.openIn fileName)
  val outs = mkOutstream Posix.FileSys.stdout
in
  while not (Deflate.endOfStream ins) do
  let val buf = Deflate.input ins in
    BinIO.output (outs, buf)
  end
end
