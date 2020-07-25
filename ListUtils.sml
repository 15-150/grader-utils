structure ListUtils =
  struct
    fun range (low : int, high_inclusive : int) : int list =
      List.tabulate (high_inclusive - low + 1, fn i => i + low)

    fun range_step (low : int, high_inclusive : int, step : int) : int list =
      List.tabulate ((high_inclusive - low + 1) div step, fn i => i * step + low)

    fun product ([] : 'a list list) : 'a list list = [[]]
      | product (l::ls : 'a list list) = (
          List.concatMap
            (fn x => (List.map (fn e => e::x) l))
            (product ls)
        )

    local
      infix 1 >>=
      fun (l : 'a list) >>= (f : 'a -> 'b list) : 'b list = List.concatMap f l

      fun pick [] = []
        | pick (x::xs) = (x, xs) :: (List.map (fn (x', xs) => (x', x::xs)) (pick xs))

      fun permutations_length (l : 'a list, 0 : int) : 'a list list = [[]]
        | permutations_length (l, r) =
            (pick l) >>= (fn (x, l') => List.map (fn l'' => x::l'') (permutations_length (l', r - 1)))
    in
      fun product_repeat (_ : 'a list, 0 : int) : 'a list list = [[]]
        | product_repeat (A, r) = product_repeat (A, r - 1) >>= (fn l => List.map (fn x => x::l) A)

      fun permutations (l : 'a list) : 'a list list = permutations_length (l, List.length l)
    end
  end
