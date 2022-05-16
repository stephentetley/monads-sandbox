namespace TestReader2 {

use Applicative.{point};
use MonadLib/Monad/Reader.Reader;
use MonadLib/Monad/Reader.{flatMap, asks, ask, local};

@test
def test01(): Unit & Impure = 
    MonadLib/Monad/Reader.runReader(point(0), "env") |> println

@test
def test02(): Unit & Impure = 
    MonadLib/Monad/Reader.runReader(Functor.map(Array.map(x -> x + 10), point([0, 1, 2, 3, 4])), "env") |> println

@test
def test03(): Unit & Impure = 
    let action = _ -> {
        let* x1 = asks(Array.get(0));
        let* x2 = asks(Array.get(1));
        point((x1, x2))
    };
    MonadLib/Monad/Reader.runReader(action(), [0, 1, 2, 3, 4]) |> println

@test
def test04(): Unit & Impure = 
    let action = _ -> {
        let* x1 = local(x -> x+2, ask());
        let* x2 = local(x -> x+1, ask());
        let* x3 = ask();
        point((x1, x2, x3))
    };
    println("Reader - should see (2, 1, 0)...");
    MonadLib/Monad/Reader.runReader(action(), 0) |> println

}    