structure Word8Buffer :> sig
  type t
  val create : int -> t
  val get : t * int -> Word8Array.array
end = struct
  type t = { buffer : Word8Array.array, p : int ref }

  fun create size = { buffer = Word8Array.array (size, 0w0), p = ref 0 }

  fun get ({buffer, p = ref p}, size) =
        let
          val dest = Word8Array.array (size, 0w00)
          fun copy (_, 0) = ()
            | copy (bufferPos, destPos) =
                let
                  val nextBufferPos =
                    if bufferPos = 0 then Word8Array.length buffer - 1
                    else bufferPos - 1
                  val nextDestPos = destPos - 1
                  val byte = Word8Array.sub (buffer, nextBufferPos)
                in
                  Word8Array.update (dest, nextDestPos, byte);
                  copy (nextBufferPos, nextDestPos)
                end
        in
          copy (p, Word8Array.length dest);
          dest
        end
end