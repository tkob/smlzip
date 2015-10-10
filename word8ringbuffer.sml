structure Word8RingBuffer :> sig
  type t
  val create : int -> t
  val put : t * Word8Array.array -> unit
  val putVec : t * Word8Vector.vector -> unit
  val putElem : t * Word8.word -> unit

  (* prev (t, sz): returns last sz bytes of the buffer *)
  val prev : t * int -> Word8Array.array
end = struct
  type t = { buffer : Word8Array.array, p : int ref }

  fun create size = { buffer = Word8Array.array (size, 0w0), p = ref 0 }

  fun put' app ({buffer, p}, arr) =
        let
          val bufferLen = Word8Array.length buffer
          fun putByte byte = (
                Word8Array.update (buffer, !p, byte);
                p := (!p + 1) mod bufferLen)
        in
          app putByte arr
        end

  val put = put' Word8Array.app;
  val putVec = put' Word8Vector.app;
  val putElem = put' (fn f => fn x => f x)

  fun prev ({buffer, p = ref p}, size) =
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
