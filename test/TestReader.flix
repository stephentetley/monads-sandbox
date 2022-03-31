namespace TestReader {

use Applicative.{point};
use MonadLib.Reader;  /// temp

@test
def test01(): Unit & Impure = 
    MonadLib/Reader.runReader(point(0), "env") |> println

@test
def test02(): Unit & Impure = 
    MonadLib/Reader.runReader(Functor.map(Array.map(x -> x + 10), point([0, 1, 2, 3, 4])), "env") |> println

@test
def test03(): Unit & Impure = 
    use MonadLib/Reader.{flatMap, asks};
    let action = _ -> {
        let* x1 = asks(Array.get(0));
        let* x2 = asks(Array.get(1));
        point((x1, x2))
    };
    MonadLib/Reader.runReader(action(), [0, 1, 2, 3, 4]) |> println

}    