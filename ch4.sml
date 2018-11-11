Control.Print.printDepth := 20;

datatype meza =
  Shrimp
| Calamari
| Escargots
| Hummus

datatype main =
  Steak
| Ravioli
| Chicken
| Eggplant

datatype salad =
  Green
| Cucumber
| Greek

datatype dessert =
  Sundae
| Mousse
| Torte

fun add_a_steak(x: meza) = (x, Steak)

fun eq_main(x: main, y: main) = x=y

fun has_steak(meza: meza, main: main, dessert: dessert) =
  main=Steak
