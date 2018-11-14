Control.Print.printDepth := 20;

datatype 'a pizza =
  Bottom
| Topping of ('a * ('a pizza))

datatype fish =
  Anchovy
| Lox
| Tuna

fun rem_anchovy (Bottom) = Bottom
  | rem_anchovy (Topping(Anchovy, p)) = rem_anchovy(p)
  | rem_anchovy (Topping(t, p)) = Topping(t, rem_anchovy(p))

fun rem_tuna (Bottom) = Bottom
  | rem_tuna (Topping(Tuna, p)) = rem_tuna(p)
  | rem_tuna (Topping(t, p)) = Topping(t, rem_tuna(p))

fun eq_fish (x: fish, y: fish) = x=y

fun rem_fish (x, Bottom) = Bottom
  | rem_fish (x, Topping(t, p)) = 
    if eq_fish(x, t)
    then rem_fish(x, p)
    else Topping(t, rem_fish(x, p))

fun subst_fish (n, a, Bottom) = Bottom
  | subst_fish (n, a, Topping(t, p)) = 
    if eq_fish(a, t)
    then Topping(n, subst_fish(n, a, p))
    else Topping(t, subst_fish(n, a, p))

fun eq_int (x: int, y: int) = x=y

fun rem_int (x, Bottom) = Bottom
  | rem_int (x, Topping(t, p)) = 
    if eq_int(x, t)
    then rem_int(x, p)
    else Topping(t, rem_int(x, p))

fun subst_int (n, a, Bottom) = Bottom
  | subst_int (n, a, Topping(t, p)) = 
    if eq_int(a, t)
    then Topping(n, subst_int(n, a, p))
    else Topping(t, subst_int(n, a, p))


datatype num = 
  Zero
| One_more_than of num

fun eq_num (Zero, Zero) = true
  | eq_num (One_more_than(x), One_more_than(y)) = eq_num(x, y)
  | eq_num (_, _) = false
