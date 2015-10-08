structure Edge :> sig
  datatype edge = Zero | One
  type edges
  val ahead : edges -> edges
  val direction : edges -> edge
  val isDeadEnd : edges -> bool
  val fromList : edge list -> edges
  val toList : edges -> edge list
  val fromWordAndLength : Word.word * int -> edges
  val toWordAndLength : edges -> Word.word * int
end = struct
  datatype edge = Zero | One
  type edges = Word.word * int

  fun ahead (w, 0) = (w, 0)
    | ahead (w, len) = (w, len - 1)

  fun direction (w, len) =
        let
          val w' = Word.>> (w, Word.fromInt (len - 1))
          val d = Word.andb (w', 0w1)
        in
          if d = 0w0 then Zero
          else One
        end

  fun isDeadEnd (_, 0) = true
    | isDeadEnd (_, _) = false

  fun lshift w = Word.<< (w, 0w1)
  fun fromList' ([], w, len) = (w, len)
    | fromList' (Zero::edges, w, len) =
        fromList' (edges, lshift w, len + 1)
    | fromList' (One::edges, w, len) =
        fromList' (edges, Word.orb (lshift w, 0w1), len + 1)

  fun fromList edges = fromList' (edges, 0w0, 0)

  fun toList edges =
        if isDeadEnd edges then []
        else
          let
            val edge = direction edges
        in
          edge::toList (ahead edges)
        end
  fun fromWordAndLength x = x
  fun toWordAndLength x = x
end
