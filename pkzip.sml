structure Pkzip :> sig
  type infile
  datatype method = Stored | Deflated
  type entry = {
    flag : word,
    method : method,
    compressedSize : int,
    uncompressedSize : int,
    fileName : string,
    offset : Position.int }
  val openIn : string -> infile
  val closeIn : infile -> unit
  val entries : infile -> entry list
  val findEntry : infile * string -> entry option
  val readEntry : infile * entry -> Word8Vector.vector
end = struct
  val fromWord8ToWord = Word.fromLarge o Word8.toLarge
  val fromWord8ToPos = Position.fromLarge o Word8.toLargeInt
  fun unpackInt vec =
        Word8Vector.foldr
          (fn (byte, int) => int * 0x100 + Word8.toInt byte)
          0
          vec
  fun unpackPos vec =
        Word8Vector.foldr
          (fn (byte, int) => int * (Position.fromInt 0x100) + fromWord8ToPos byte)
          (Position.fromInt 0)
          vec
  fun unpackWord vec =
        Word8Vector.foldr
          (fn (byte, word) =>
            Word.orb (Word.<< (word, Word.fromInt 8),fromWord8ToWord byte))
          0w0
          vec
  val b = Byte.stringToBytes
  val s = Byte.bytesToString

  datatype method = Stored | Deflated

  type entry = {
    flag : word,
    method : method,
    compressedSize : int,
    uncompressedSize : int,
    fileName : string,
    offset : Position.int }

  type infile = BinRandomAccessFile.infile * entry list

  fun readInt2 infile = unpackInt (BinRandomAccessFile.read (infile, 2))
  fun readInt4 infile = unpackInt (BinRandomAccessFile.read (infile, 4))
  fun readPos4 infile = unpackPos (BinRandomAccessFile.read (infile, 4))
  fun readWord2 infile = unpackWord (BinRandomAccessFile.read (infile, 2))
  fun readString (infile, n) = s (BinRandomAccessFile.read (infile, n))
  fun readMethod infile =
        case readInt2 infile of
             0 => Stored
           | 8 => Deflated
           | m =>
               raise Fail ("Unsupported compression method: " ^ Int.toString m)

  fun readLocalFileHeader (infile, {offset, ...} : entry) =
        let
          open BinRandomAccessFile
          val _ = seekIn (infile, offset)
          val version          = read (infile, 2)
          val flag             = readWord2 infile
          val encrypted = Word.andb (flag, 0wx0001) = 0wx0001
          val hasDataDesc = Word.andb (flag, 0wx0008) = 0wx0001
          val _ = if hasDataDesc then raise Fail "data descriptor not supported"
                  else ()
          val method           = readMethod infile
          val time             = read (infile, 2)
          val date             = read (infile, 2)
          val crc32            = read (infile, 4)
          val compressedSize   = readInt4 infile
          val uncompressedSize = readInt4 infile
          val fileNameLength   = readInt2 infile
          val extraFieldLength = readInt2 infile
          val fileName         = readString (infile, fileNameLength)
          val extraField       = read (infile, extraFieldLength)
          val fileData         = read (infile, compressedSize)
        in
          fileData
        end

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
            val compression_method              = readMethod infile
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
            val relative_offset_of_local_header = readPos4 infile
            val file_name    = readString (infile, file_name_length)
            val extra_field  = read (infile, extra_field_length)
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
          val len = Position.toLarge (endPosIn infile)
          val bufSize = min (len, 65557)
          val bufPos = len - bufSize
          val _ = seekIn (infile, Position.fromLarge bufPos)
          val buf = read (infile, Int.fromLarge bufSize)
          val posEcd =
            case findLast {text = buf, pattern = b "PK\005\006"} of
                 NONE => raise Fail "not a PKZIP file"
               | SOME posInBuf => Position.fromLarge (bufPos + Int.toLarge posInBuf)
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
          val offset_of_start_of_cd_wrt_starting_disk_num = readPos4 infile
          val zip_file_comment_len                        = readInt2 infile
          val zip_file_comment = readString (infile, zip_file_comment_len)
          (* read entries in central directory *)
          val _ = seekIn (infile, offset_of_start_of_cd_wrt_starting_disk_num)
          val entries = readCd (infile, posEcd, total_num_of_entries_in_cd, [])
        in
          if num_of_this_disk <> 0 orelse num_of_disk_with_start_of_cd <> 0 then
            raise Fail "multiple disks not supported"
          else
            entries
        end

  fun openIn fileName =
        let
          val infile = BinRandomAccessFile.openIn fileName
          val entries = locateEndOfCentralDirectory infile
            handle e => raise e before BinRandomAccessFile.closeIn infile
        in
          (infile, entries)
        end

  fun entries (infile, entries) = entries

  fun findEntry ((_, entries : entry list), fileName) =
        let
          fun find [] = NONE
            | find (entry::entries) =
                if #fileName entry = fileName then SOME entry
                else find entries
        in
          find entries
        end

  fun readEntry (infile, entry) =
        raise Fail "unimplemented"

  fun closeIn (infile, _) = BinRandomAccessFile.closeIn infile
end
