let take i xs =
  let rec work i acc = function
    | _ when i = 0 -> acc
    | x :: xs -> work (i - 1) (x :: acc) xs
    | _ -> invalid_arg "take"
  in work i [] xs

let rec drop i = function
  | _ when i < 0 -> invalid_arg "drop"
  | [] -> invalid_arg "drop"
  | xs when i = 0 -> xs
  | _ :: xs -> drop (i - 1) xs

