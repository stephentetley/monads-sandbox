
namespace TestFoldableExtras {


use Applicative.{point};
use MonadLib/Monad.ContWriter;
use MonadLib/Monad/ContWriter.{runContWriter, tell};

@test
def test01(): Unit & Impure =  
    let xs = makeList(2000001);
    let ans1 = FoldableExtras.foldLeftM((_, x) -> Some(ToString.toString(x)), "", xs);  
    println("length(xs): ${List.length(xs)}, ans1: ${ans1}");

    let ans2 = FoldableExtras.foldRightM((x, _) -> Some(ToString.toString(x)), "", xs);  
    println("length(xs): ${List.length(xs)}, ans2: ${ans2}");
    ()

def makeList(size: Int32): List[Int32] = 
    List.range(1, size)

@test
def test02(): Unit & Impure =     
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

}    