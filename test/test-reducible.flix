

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    Reducible.reduce(Nel("a", "b" :: "c" :: "d" :: Nil)) |> println;
    Reducible.reduceLeftTo(ToString.toString, (b, a) -> "${b}${a}", Nel(1, 2 :: 3 :: 4 :: Nil)) |> println;
    Reducible.reduceRightTo(ToString.toString, (a, b) -> "${b}${a}", Nel(1, 2 :: 3 :: 4 :: Nil)) |> println;
    0