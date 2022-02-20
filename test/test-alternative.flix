

def main(_args: Array[String]): Int32 & Impure = 
    println("Running");
    test01() |> println;
    0

def test01(): Unit & Impure = 
    use Alternative.{<|>};
    None <|> Some(1) |> println;
    ()
