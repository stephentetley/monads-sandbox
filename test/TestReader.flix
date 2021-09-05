use MonadLib.Reader;  /// temp


def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    test01();
    test02();
    test03();
    0


def test01(): Unit & Impure = 
    use MonadLib/Reader.{return};
    MonadLib/Reader.runReader(return(0), "env") |> println

def test02(): Unit & Impure = 
    use MonadLib/Reader.{return};
    MonadLib/Reader.runReader(Functor.map(Array.map(x -> x + 10), Applicative.point([0, 1, 2, 3, 4])), "env") |> println


def test03(): Unit & Impure = 
    use MonadLib/Reader.{flatMap, asks, return};
    let action = _ -> {
        let* x1 = asks(flip(Array.get, 0));
        let* x2 = asks(flip(Array.get, 1));
        return((x1, x2))
    };
    MonadLib/Reader.runReader(action(), [0, 1, 2, 3, 4]) |> println