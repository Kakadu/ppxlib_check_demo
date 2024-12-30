let __1 : _ = fun _ -> 1

let () =
  let ( let* ) x f = f x in
  let* x = "asdf" in
  print_string x
