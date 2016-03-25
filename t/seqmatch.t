# Setup

```
- use "seqmatch.sml";
...
val it = () : unit
- val b = Byte.stringToBytes;
...
val b = fn : string -> Word8Vector.vector
```

# StringMatch

```
- StringMatch.findFirst {pattern = "abc", text = "aaaabcbcbcabcx"};
val it = SOME 3 : int option
- StringMatch.findFirst {pattern = "", text = "aaaabcbcbcabcx"};
val it = SOME 0 : int option
- StringMatch.findFirst {pattern = "abcx", text = "aaaabcbcbcabcx"};
val it = SOME 10 : int option
- StringMatch.findFirst {pattern = "abc", text = ""};
val it = NONE : int option
- StringMatch.findLast {pattern = "abc", text = "aaaabcbcbcabcx"};
val it = SOME 10 : int option
- StringMatch.findLast {pattern = "", text = "aaaabcbcbcabcx"};
val it = SOME 14 : int option
- StringMatch.findLast {pattern = "aaaa", text = "aaaabcbcbcabcx"};
val it = SOME 0 : int option
- StringMatch.findLast {pattern = "abc", text = ""};
val it = NONE : int option
- StringMatch.findAll {pattern = "abc", text = "aaaabcbcbcabcx"};
val it = [3,10] : int list
- StringMatch.findAll {pattern = "", text = "aa"};
val it = [0,1,2] : int list
- StringMatch.findAll {pattern = "a", text = "b"};
val it = [] : int list
- StringMatch.findAll {pattern = "", text = ""};
val it = [0] : int list
```

# Word8VectorMatch

```
- Word8VectorMatch.findFirst {pattern = b "abc", text = b "aaaabcbcbcabcx"};
val it = SOME 3 : int option
- Word8VectorMatch.findFirst {pattern = b "", text = b "aaaabcbcbcabcx"};
val it = SOME 0 : int option
- Word8VectorMatch.findFirst {pattern = b "abcx", text = b "aaaabcbcbcabcx"};
val it = SOME 10 : int option
- Word8VectorMatch.findFirst {pattern = b "abc", text = b ""};
val it = NONE : int option
- Word8VectorMatch.findLast {pattern = b "abc", text = b "aaaabcbcbcabcx"};
val it = SOME 10 : int option
- Word8VectorMatch.findLast {pattern = b "", text = b "aaaabcbcbcabcx"};
val it = SOME 14 : int option
- Word8VectorMatch.findLast {pattern = b "aaaa", text = b "aaaabcbcbcabcx"};
val it = SOME 0 : int option
- Word8VectorMatch.findLast {pattern = b "abc", text = b ""};
val it = NONE : int option
- Word8VectorMatch.findAll {pattern = b "abc", text = b "aaaabcbcbcabcx"};
val it = [3,10] : int list
- Word8VectorMatch.findAll {pattern = b "", text = b "aa"};
val it = [0,1,2] : int list
- Word8VectorMatch.findAll {pattern = b "a", text = b "b"};
val it = [] : int list
- Word8VectorMatch.findAll {pattern = b "", text = b ""};
val it = [0] : int list
```
