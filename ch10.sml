Control.Print.printDepth := 20;

fun eq_int(a:int, b:int) = a=b

signature N = 
  sig
    type number
    exception Too_small
    val succ: number -> number
    val pred: number -> number
    val is_zero: number -> bool
  end

(* (* num version *) *)
(* functor NumberAsNum() :> N = *)
(*   struct *)
(*     datatype num = *)
(*       Zero *)
(*     | One_more_than of num *)
(*     type number = num *)
(*     exception Too_small *)
(*     fun succ(n) = One_more_than(n) *)
(*     fun pred (Zero) = raise Too_small *)
(*       | pred (One_more_than(n)) = n *)
(*     fun is_zero (Zero) = true *)
(*       | is_zero (_) = false *)
(*   end *)

(* (* int version *) *)
(* functor NumberAsInt() :> N = *)
(*   struct *)
(*     type number = int *)
(*     exception Too_small *)
(*     fun succ(n) = n + 1 *)
(*     fun pred(n) = *)
(*         if eq_int(n, 0) *)
(*         then raise Too_small *)
(*         else n - 1 *)
(*     fun is_zero(n) = eq_int(n, 0) *)
(*   end *)


signature N_C_R =
  sig
    type number
    exception Too_small
    val conceal: int -> number
    val succ: number -> number
    val pred: number -> number
    val is_zero: number -> bool
    val reveal: number -> int
  end

(* (* num version *) *)
(* functor NumberAsNum() :> N_C_R = *)
(*   struct *)
(*     datatype num = *)
(*       Zero *)
(*     | One_more_than of num *)
(*     type number = num *)
(*     exception Too_small *)
(*     fun conceal(n) =n *)
(*         if eq_int(n, 0) *)
(*         then Zero *)
(*         else One_more_than(conceal(n-1)) *)
(*     fun succ(n) = One_more_than(n) *)
(*     fun pred (Zero) = raise Too_small *)
(*       | pred (One_more_than(n)) = n *)
(*     fun is_zero (Zero) = true *)
(*       | is_zero (_) = false *)
(*     fun reveal(n) = *)
(*         if is_zero(n) *)
(*         then 0 *)
(*         else 1 + reveal(pred(n)) *)
(*   end *)

(* (* int version *) *)
(* functor NumberAsInt() :> N_C_R = *)
(*   struct *)
(*     type number = int *)
(*     exception Too_small *)
(*     fun conceal(n) = n *)
(*     fun succ(n) = n + 1 *)
(*     fun pred(n) = *)
(*         if eq_int(n, 0) *)
(*         then raise Too_small *)
(*         else n - 1 *)
(*     fun is_zero(n) = eq_int(n, 0) *)
(*     fun reveal(n) = n *)
(*   end *)

signature P =
  sig
    type number
    val plus: (number * number) -> number
  end

functor PON (structure a_N: N) 
  :> 
  P where type number = a_N.number
  =
  struct
    type number = a_N.number
    fun plus (n, m) =
      if a_N.is_zero(n)
      then m
      else a_N.succ(plus(a_N.pred(n), m))
  end

structure IntStruct = NumberAsInt()
structure IntArith = PON(structure a_N = IntStruct)
structure NumStruct = NumberAsNum()
structure NumArith = PON(structure a_N = NumStruct)

signature S =
  sig
    type number1
    type number2
    val similar:
        (number1 * number2) -> bool
  end

functor Same(structure a_N: N
             structure b_N: N)
  :>
  S where type number1 = a_N.number
    where type number2 = b_N.number
  =
  struct
    type number1 = a_N.number
    type number2 = b_N.number
    fun sim(n,m) =
        if a_N.is_zero(n)
        then b_N.is_zero(m)
        else sim(a_N.pred(n),
                 b_N.pred(m))
    fun similar(n,m) =
        ((sim(n,m)
          handle a_N.Too_small => false)
          handle b_N.Too_small => false)
  end

structure SimIntNum = Same(structure a_N = IntStruct
                           structure b_N = NumStruct)
structure SimNumInt = Same(structure a_N = NumStruct
                           structure b_N = IntStruct)

fun new_plus(x,y)
  = NumStruct.reveal(
      NumArith.plus(
        NumStruct.conceal(x),
        NumStruct.conceal(y)))

signature J =
  sig
    val new_plus: 
        int * int -> int
  end

functor NP(structure a_N : N_C_R
           structure a_P : P
           sharing type
             a_N.number
             =
             a_P.number)
  :>
  J
  =
  struct
    fun new_plus(x,y) =
      a_N.reveal(
        a_P.plus(
          a_N.conceal(x),
          a_N.conceal(y)))
  end

structure NPStruct = NP(structure a_N = NumStruct
                        structure a_P = NumArith)


signature T =
  sig
    type number
    val times : number*number -> number
  end

functor TON(structure a_N : N
            structure a_P : P
            sharing type
              a_N.number
              =
              a_P.number)
  :>
  T where type number = a_N.number
  =
  struct
    type number = a_N.number
    fun times(n,m) =
      if a_N.is_zero(m)
      then m
      else a_P.plus(n, times(n,a_N.pred(m)))
  end

structure TONStruct = TON(structure a_N = NumberAsNum()
                          structure a_P = PON(structure a_N = a_N))
