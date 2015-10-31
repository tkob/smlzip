functor SeqMatch(type seq;
                 type slice;
                 val size : seq -> int;
                 val slice : seq * int * int -> slice;
                 val compare : slice * slice -> order) :> sig
  val findFirst : {text : seq, pattern : seq} -> int option
  val findLast  : {text : seq, pattern : seq} -> int option
  val findAll : {text : seq, pattern : seq} -> int list
end = struct
  fun find' (all, t, p, m, next, isFinal, s, ps) =
        if isFinal s then rev ps
        else
          let
            val t' = slice (t, s, m)
            val s' = next s
          in
            if compare(t', p) = EQUAL
            then
              if all then find' (all, t, p, m, next, isFinal, s', s::ps)
              else [s]
            else find' (all, t, p, m, next, isFinal, s', ps)
          end

  fun incr s = s + 1
  fun decr s = s - 1
  fun hd' [] = NONE
    | hd' (x::xs) = SOME x

  fun findAll {text, pattern} =
        let
          val m = size pattern
          val final = size text - m
          fun isFinal s = s > final
        in
          find' (true, text, slice (pattern, 0, m), m, incr, isFinal, 0, [])
        end

  fun findFirst {text, pattern} =
        let
          val m = size pattern
          val final = size text - m
          fun isFinal s = s > final
          val found = find' (false, text, slice (pattern, 0, m), m, incr, isFinal, 0, [])
        in
          hd' found
        end

  fun findLast {text, pattern} =
        let
          val m = size pattern
          val start = size text - m
          fun isFinal s = s < 0
          val found = find' (false, text, slice (pattern, 0, m), m, decr, isFinal, start, [])
        in
          hd' found
        end
end

structure StringMatch = SeqMatch(
  type seq = string;
  type slice = Substring.substring;
  val size = String.size;
  val slice = Substring.substring;
  val compare = Substring.compare)

structure Word8VectorMatch = SeqMatch(
  type seq = Word8Vector.vector;
  type slice = Word8VectorSlice.slice;
  val size = Word8Vector.length;
  val slice = fn (v, s, e) => Word8VectorSlice.slice (v, s, SOME e);
  val compare = Word8VectorSlice.collate Word8.compare)

(* unit tests *)
local open StringMatch in
  val SOME 3 = findFirst {pattern = "abc", text = "aaaabcbcbcabcx"}
  val SOME 0 = findFirst {pattern = "", text = "aaaabcbcbcabcx"}
  val SOME 10 = findFirst {pattern = "abcx", text = "aaaabcbcbcabcx"}
  val NONE = findFirst {pattern = "abc", text = ""}
  val SOME 10 = findLast {pattern = "abc", text = "aaaabcbcbcabcx"}
  val SOME 14 = findLast {pattern = "", text = "aaaabcbcbcabcx"}
  val SOME 0 = findLast {pattern = "aaaa", text = "aaaabcbcbcabcx"}
  val NONE = findLast {pattern = "abc", text = ""}
  val [3, 10] = findAll {pattern = "abc", text = "aaaabcbcbcabcx"}
  val [0, 1, 2] = findAll {pattern = "", text = "aa"}
  val [] = findAll {pattern = "a", text = "b"}
  val [0] = findAll {pattern = "", text = ""}
end

local open Word8VectorMatch in
  val b = Byte.stringToBytes
  val SOME 3 = findFirst {pattern = b "abc", text = b "aaaabcbcbcabcx"}
  val SOME 0 = findFirst {pattern = b "", text = b "aaaabcbcbcabcx"}
  val SOME 10 = findFirst {pattern = b "abcx", text = b "aaaabcbcbcabcx"}
  val NONE = findFirst {pattern = b "abc", text = b ""}
  val SOME 10 = findLast {pattern = b "abc", text = b "aaaabcbcbcabcx"}
  val SOME 14 = findLast {pattern = b "", text = b "aaaabcbcbcabcx"}
  val SOME 0 = findLast {pattern = b "aaaa", text = b "aaaabcbcbcabcx"}
  val NONE = findLast {pattern = b "abc", text = b ""}
  val [3, 10] = findAll {pattern = b "abc", text = b "aaaabcbcbcabcx"}
  val [0, 1, 2] = findAll {pattern = b "", text = b "aa"}
  val [] = findAll {pattern = b "a", text = b "b"}
  val [0] = findAll {pattern = b "", text = b ""}
end
