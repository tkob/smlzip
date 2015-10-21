structure Tree = struct
  datatype 'a t = Leaf of 'a | Node of 'a t * 'a t | None
  type 'a path = Edge.edges * 'a

  fun ahead (edges, value) = (Edge.ahead edges, value)

  fun make [(edges, value)] =
        if Edge.isDeadEnd edges then Leaf value
        else raise Fail "not dead end at leaf"
    | make [] = None
    | make paths =
        let
          fun split (path as (edges, _), (zero, one)) = 
                if Edge.isDeadEnd edges then (zero, one)
                else
                  case Edge.direction edges of
                       Edge.Zero => (path::zero, one)
                     | Edge.One => (zero, path::one)
          val (zero, one) = List.foldr split ([], []) paths
        in
          Node (make (map ahead zero), make (map ahead one))
        end

  fun follow (Leaf value, edges) =
        if Edge.isDeadEnd edges then value
        else raise Fail "reached dead end"
    | follow (None, edges) = raise Fail "cannot follow None"
    | follow (Node (zero, one), edges) =
        case Edge.direction edges of
             Edge.Zero => follow (zero, Edge.ahead edges)
           | Edge.One => follow (one, Edge.ahead edges)

  fun zero (Leaf _) = raise Fail "leaf"
    | zero None = raise Fail "none"
    | zero (Node (zero, _)) = zero
  fun one (Leaf _) = raise Fail "leaf"
    | one None = raise Fail "none"
    | one (Node (_, one)) = one

  fun show toString (Leaf value) = toString value
    | show toString None = "none"
    | show toString (Node (zero, one)) =
        "(" ^ show toString zero ^ ", " ^ show toString one ^ ")"
end
