structure Pkzip = struct
  val fromWord8ToWord = Word.fromLarge o Word8.toLarge
  fun unpackInt vec =
        Word8Vector.foldr
          (fn (byte, int) => int * 0x100 + Word8.toInt byte)
          0
          vec
  fun unpackWord vec =
        Word8Vector.foldr
          (fn (byte, word) =>
            Word.orb (Word.<< (word, Word.fromInt 8),fromWord8ToWord byte))
          0w0
          vec
  val b = Byte.stringToBytes
  val s = Byte.bytesToString

  fun readLocalFileHeader ins =
        let
          val version = BinIO.inputN (ins, 2)
          val flag = unpackWord (BinIO.inputN (ins, 2))
          val encrypted = Word.andb (flag, 0wx0001) = 0wx0001
          val hasDataDesc = Word.andb (flag, 0wx0008) = 0wx0001
          val _ = if hasDataDesc then raise Fail "data descriptor not supported"
                  else ()
          val method = unpackInt (BinIO.inputN (ins, 2))
          val time = BinIO.inputN (ins, 2)
          val date = BinIO.inputN (ins, 2)
          val crc32 = BinIO.inputN (ins, 4)
          val compressedSize = unpackInt (BinIO.inputN (ins, 4))
          val uncompressedSize = unpackInt (BinIO.inputN (ins, 4))
          val fileNameLength = unpackInt (BinIO.inputN (ins, 2))
          val extraFieldLength = unpackInt (BinIO.inputN (ins, 2))
          val fileName = Byte.bytesToString (BinIO.inputN (ins, fileNameLength))
          val extraField = BinIO.inputN (ins, extraFieldLength)
          val fileData = BinIO.inputN (ins, compressedSize)
        in
          { flag = flag,
            method = method,
            compressedSize = compressedSize,
            uncompressedSize = uncompressedSize,
            fileName = fileName,
            fileData = fileData }
        end

  fun parseIn ins files =
        let
          val s = BinIO.inputN (ins, 4)
        in
          if s = b "PK\003\004" then
            let val localFile = readLocalFileHeader ins in
              parseIn ins (localFile::files)
            end
          else
            rev files
        end

  fun openIn name =
        let
          val ins = BinIO.openIn name 
        in
          parseIn ins [] before BinIO.closeIn ins
        end
end
