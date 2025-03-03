let () =
  let my_set = ListSet.set_of_list (Cons (5, Nil ())) in
  let _ = ListSet.add (6, my_set) in
  ()
