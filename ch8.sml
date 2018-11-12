Control.Print.printDepth := 20;

datatype 'a list =
  Empty
| Cons of 'a * 'a list

datatype orapl =
  Orange
| Apple

fun eq_orapl(a: orapl, b: orapl) = a=b
fun eq_int(a: int, b: int) = a=b


fun subst_int (n, a, Empty) = Empty
  | subst_int (n, a, Cons(e, t)) = 	
	if eq_int(a, e)
	then Cons(n, subst_int(n, a, t))
	else Cons(e, subst_int(n, a, t))

fun subst_orapl (n, a, Empty) = Empty
  | subst_orapl (n, a, Cons(e, t)) = 
	if eq_orapl(a, e)
	then Cons(n, subst_orapl(n, a, t))
	else Cons(e, subst_orapl(n, a, t))

fun subst (rel, n, a, Empty) = Empty
  | subst (rel, n, a, Cons(e, t)) = 
	if rel(a,e)
	then Cons(n, subst(rel, n, a, t))
	else Cons(e, subst(rel, n, a, t))

fun in_range((small, large), x) =
	x > small andalso x < large

fun subst_pred (pred, n, Empty) = Empty
  | subst_pred (pred, n, Cons(e, t)) = 
	if pred(e)
	then Cons(n, subst_pred(pred, n, t))
	else Cons(e, subst_pred(pred, n, t))

fun is_15(n) = eq_int(n, 15)
fun less_than_15(n) = n < 15

fun in_range_c(small, large)(x) = small < x andalso x < large

fun subst_c (pred)(n, Empty) = Empty
  | subst_c (pred)(n, Cons(e, t)) = 
	if pred(e)
	then Cons(n, subst_c (pred)(n, t))
	else Cons(e, subst_c (pred)(n, t))

fun subst_c_in_range_11_16 (n, Empty) = Empty
  | subst_c_in_range_11_16 (n, Cons(e, t)) = 
	if in_range_c(11,16)(e)
	then Cons(n, subst_c_in_range_11_16(n, t))
	else Cons(e, subst_c_in_range_11_16(n, t))

fun combine (Empty, Empty) = Empty
  | combine (Empty, Cons(b,l2)) = Cons(b,l2)
  | combine (Cons(a,l1), Empty) = Cons(a,l1)
  | combine (Cons(a,l1), Cons(b,l2)) = Cons(a, combine(l1,Cons(b,l2)))


