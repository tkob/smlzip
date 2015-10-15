structure Word8Buffer :> sig
  type t
  val create : int -> t
  val putElem : t * Word8.word -> unit
  val isFull : t -> bool
  val isEmpty : t -> bool
  val freeze : t -> Word8Vector.vector
  val init : t -> unit
  val length : t -> int
end = struct
  type t = { buffer : Word8Array.array, p : int ref }

  fun create size = { buffer = Word8Array.array (size, 0w0), p = ref 0 }

  fun putElem ({buffer, p}, byte) = (
        Word8Array.update (buffer, !p, byte);
        p := (!p + 1))

  fun isFull {buffer, p} = !p >= Word8Array.length buffer
  fun isEmpty {buffer, p} = !p = 0

  fun freeze {buffer, p} =
        let
          open Word8ArraySlice
        in
          vector (slice (buffer, 0, SOME (!p)))
        end

  fun init {buffer, p} = p := 0

  fun length {buffer, p} = !p
end
