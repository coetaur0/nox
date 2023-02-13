(* ----- Compiler environment ------------------------------------------------------------------- *)

include Map.Make (String)

let of_list list = list |> List.to_seq |> of_seq

let merge lhs rhs =
  merge
    (fun _ left_elt right_elt ->
      match (left_elt, right_elt) with
      | (Some left_value, Some _) -> Some left_value
      | (Some left_value, None) -> Some left_value
      | (None, Some right_value) -> Some right_value
      | _ -> assert false )
    lhs rhs
