structure Pkzip :> sig
  type pkzip
  type entry = {
    flag : word,
    method : int,
    compressedSize : int,
    uncompressedSize : int,
    fileName : string,
    offset : int }
  val openIn' : string -> pkzip
  val entries : pkzip -> entry list
  val closeIn : pkzip -> unit
end = struct
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

  type entry = {
    flag : word,
    method : int,
    compressedSize : int,
    uncompressedSize : int,
    fileName : string,
    offset : int }

  type pkzip = BinRandomAccessFile.infile * entry list

  fun readInt2 infile = unpackInt (BinRandomAccessFile.read (infile, 2))
  fun readInt4 infile = unpackInt (BinRandomAccessFile.read (infile, 4))
  fun readWord2 infile = unpackWord (BinRandomAccessFile.read (infile, 2))
  fun readString (infile, n) = s (BinRandomAccessFile.read (infile, n))

  fun readCd (infile, posEcd, 0, entries) = rev entries
    | readCd (infile, posEcd, numEntries, entries) =
        if BinRandomAccessFile.tellIn infile >= posEcd then raise Fail ""
        else
          let
            open BinRandomAccessFile
            val central_file_header_signature   = read (infile, 4)
            val _  =
              if central_file_header_signature = b "PK\001\002"
              then ()
              else raise Fail ""
            val version_made_by                 = read (infile, 2)
            val version_needed_to_extract       = read (infile, 2)
            val general_purpose_bit_flag        = readWord2 infile
            val compression_method              = readInt2 infile
            val last_mod_file_time              = read (infile, 2)
            val last_mod_file_date              = read (infile, 2)
            val crc32                           = read (infile, 4)
            val compressed_size                 = readInt4 infile
            val uncompressed_size               = readInt4 infile
            val file_name_length                = readInt2 infile
            val extra_field_length              = readInt2 infile
            val file_comment_length             = readInt2 infile
            val disk_number_start               = readInt2 infile
            val internal_file_attributes        = read (infile, 2)
            val external_file_attributes        = read (infile, 4)
            val relative_offset_of_local_header = readInt4 infile
            val file_name = readString (infile, file_name_length)
            val extra_field = read (infile, extra_field_length)
            val file_comment = readString (infile, file_comment_length)
            val entry = {
                  flag = general_purpose_bit_flag,
                  method = compression_method,
                  compressedSize = compressed_size,
                  uncompressedSize = uncompressed_size,
                  fileName = file_name,
                  offset = relative_offset_of_local_header }
          in
            readCd (infile, posEcd, numEntries - 1, entry::entries)
          end

  fun locateEndOfCentralDirectory infile =
        let
          open BinRandomAccessFile
          open Word8VectorMatch
          fun min (a, b) = if a < b then a else b
          (* first, locate ECD position (=> posEcd) *)
          val len = Position.toInt (endPosIn infile)
          val bufSize = min (len, 65557)
          val bufPos = len - bufSize
          val _ = seekIn (infile, Position.fromInt bufPos)
          val buf = read (infile, bufSize)
          val posEcd =
            case findLast {text = buf, pattern = b "PK\005\006"} of
                 NONE => raise Fail "not a PKZIP file"
               | SOME posInBuf => Position.fromInt (bufPos + posInBuf)
          (* next, read ECD *)
          val _ = seekIn (infile, posEcd)
          val end_of_cd_signature                         = read (infile, 4)
          val _  =
            if end_of_cd_signature = b "PK\005\006" then ()
            else raise Fail "cannot locate end of central directory"
          val num_of_this_disk                            = readInt2 infile
          val num_of_disk_with_start_of_cd                = readInt2 infile
          val total_num_of_entries_in_cd_on_this_disk     = readInt2 infile
          val total_num_of_entries_in_cd                  = readInt2 infile
          val size_of_cd                                  = readInt4 infile
          val offset_of_start_of_cd_wrt_starting_disk_num = readInt4 infile
          val zip_file_comment_len                        = readInt2 infile
          val zip_file_comment = readString (infile, zip_file_comment_len)
          (* read entries in central directory *)
          val _ = seekIn (infile, Position.fromInt offset_of_start_of_cd_wrt_starting_disk_num)
          val entries = readCd (infile, posEcd, total_num_of_entries_in_cd, [])
        in
          if num_of_this_disk <> 0 orelse num_of_disk_with_start_of_cd <> 0 then
            raise Fail "multiple disks not supported"
          else
            entries
        end

  fun openIn' fileName =
        let
          val infile = BinRandomAccessFile.openIn fileName
          val entries = locateEndOfCentralDirectory infile
            handle e => raise e before BinRandomAccessFile.closeIn infile
        in
          (infile, entries)
        end

  fun entries (infile, entries) = entries

  fun closeIn (infile, _) = BinRandomAccessFile.closeIn infile
end
