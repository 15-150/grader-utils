functor MkRandomUtils (val seed : int * int) =
  struct
    val RAND = Random.rand seed  (* mutable *)

    fun int () : int = Random.randInt RAND

    fun int_range (range as (low : int, high_inclusive : int)) : int =
      Random.randRange range RAND

    fun ints_range (range as (low : int, high_inclusive : int)) (n : int) : int list =
      List.tabulate (n, fn _ => int_range range)

    fun coin () : bool = int_range (0, 1) = 1

    (* inspired from http://stackoverflow.com/a/27248080/1266600 *)
    fun shuffle (l : 'a list) : 'a list =
      let
        fun select (y::xs, 0) = (y, xs)
          | select (x::xs, i) = let val (y, xs') = select (xs, i - 1) in (y, x::xs') end
          | select (_, i) = raise Fail ("Short by " ^ Int.toString i ^ " elements.")

        fun rtake [] _ = []
          | rtake ys max =
            let
              val (y, ys') = select (ys, Random.randRange (0, max - 1) RAND)
            in
              y :: rtake ys' (max - 1)
            end
      in
        rtake l (length l)
      end

    fun choice (l : 'a list) : 'a =
      List.nth (l, Random.randRange (0, (List.length l) - 1) RAND)

    fun unique_choices (l : 'a list, n : int) : 'a list = List.take (shuffle l, n)

    fun sublist (l : 'a list) = unique_choices (l, int_range (0, List.length l - 1))
  end
