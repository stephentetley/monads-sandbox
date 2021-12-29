use Applicative.{point};
use MonadLib/Monad.ContWriter;
use MonadLib/Monad/ContWriter.{runContWriter, tell};

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    test01();
    0

def test01(): Unit & Impure =     
    let xs = 1 :: 2 :: 3 :: 4 :: Nil;
    println("foreachM");
    runContWriter(FoldableExtras.foreachM(Chain.singleton >> tell, xs)) |> println;

    println("foldLeftM");
    runContWriter(FoldableExtras.foldLeftM(foldStep, 0, xs)) |> println;

    println("foldRightM");
    runContWriter(FoldableExtras.foldRightM(flip(foldStep), 0, xs)) |> println;
    ()


def foldStep(b: Int32, a: Int32): ContWriter[ka, Chain[(Int32, Int32)], Int32] = 
    use MonadLib/Monad/ContWriter.{flatMap};
    let acc = b + a;
    let* _ = tell(Chain.singleton((a, acc)));
    point(acc)

    