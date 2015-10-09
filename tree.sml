structure Tree = struct
  datatype 'a t = Leaf of 'a | Node of 'a t * 'a t
  type 'a path = Edge.edges * 'a

  fun ahead (edges, value) = (Edge.ahead edges, value)

  fun make [(edges, value)] =
        if Edge.isDeadEnd edges then Leaf value
        else raise Fail "paths are not binary-branching"
    | make paths =
        let
          fun split (path as (edges, _), (zero, one)) = 
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
    | follow (Node (zero, one), edges) =
        case Edge.direction edges of
             Edge.Zero => follow (zero, Edge.ahead edges)
           | Edge.One => follow (one, Edge.ahead edges)

  fun zero (Leaf _) = raise Fail "leaf"
    | zero (Node (zero, _)) = zero
  fun one (Leaf _) = raise Fail "leaf"
    | one (Node (_, one)) = one

  fun show toString (Leaf value) = toString value
    | show toString (Node (zero, one)) =
        "(" ^ show toString zero ^ ", " ^ show toString one ^ ")"
end
