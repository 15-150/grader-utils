structure GRADER_UTILS =
  struct
    open ListUtils

    local
      infix 1 >>=
      fun (x : 'a option) >>= (f : 'a -> 'b option) = Option.mapPartial f x
      val seedOpt =
        OS.Process.getEnv "GRADER_UTILS_SEED"
          >>= SOME o String.fields (Fn.curry (op =) #",")
          >>= (fn [n1,n2] => SOME (n1,n2) | _ => NONE)
          >>= SOME o (fn (n1,n2) => (Int.fromString n1, Int.fromString n2))
          >>= (fn (SOME n1,SOME n2) => SOME (n1,n2) | _ => NONE)
    in
      structure Random =
        MkUtils (
          val seed = (
            case seedOpt of
              NONE      => raise Fail "GRADER_UTILS_SEED not defined"
            | SOME seed => seed
          )
        )
    end
  end
