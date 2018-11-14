Control.Print.printDepth := 20;

datatype 'a list =
  Empty
| Cons of 'a * 'a list

datatype box =
  Bacon
| Ix of int

fun is_bacon (Bacon) = true
  | is_bacon (Ix(n)) = false

exception No_bacon of int

fun where_is (Empty) = raise No_bacon(0)
  | where_is (Cons(a_box, rest)) = 
	if is_bacon(a_box)
	then 1
	else 1 + where_is(rest)

fun eq_int(a: int, b: int) = a=b
exception Out_of_range

fun list_item (n, Empty) = raise Out_of_range
  | list_item (n, Cons(abox, rest)) = 
	if eq_int(n, 1)
	then abox
	else list_item(n - 1, rest)

fun find(n, boxes) = 
	check(n, boxes, list_item(n, boxes))
	handle Out_of_range => find(n div 2, boxes)
and
	check (n, boxes, Bacon) = n
  | check (n, boxes, Ix(i)) = find(i, boxes)

(* variable used to test 'find' *)
val t = Cons(Ix(5),
			 Cons(Ix(4),
				 Cons(Bacon,
					  Cons(Ix(2),
						   Cons(Ix(7),
								Empty)))))


fun path (n, boxes) = 
	Cons(n,
		check(boxes, list_item(n, boxes))
		handle Out_of_range => path(n div 2, boxes))
and 
	check (boxes, Bacon) = Empty
  | check (boxes, Ix(i)) = path(i, boxes)
