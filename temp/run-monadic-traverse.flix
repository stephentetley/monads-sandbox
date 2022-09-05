

def main(): Int32 & Impure = 
    println("run-monadic-traverse...");
    t01() |> Result.map(List.maximum) |> println;
    t02() |> Result.map(List.maximum) |> println;
    0

def t01(): Result[List[Int32], String] = 
    use Applicative.{point};
    use MonadsSandbox/MonadicTraverse.{traverseM};
    use MonadsSandbox/ErrorMonad.{runError, throwError};
    traverseM(x -> if (x >= 0) point(x) else throwError("too small"), List.range(0, 10000001)) |> runError



def t02(): Result[List[Int32], String] \ IO = 
    use Applicative.{point};
    use MonadsSandbox/MonadicTraverse.{traverseM};
    use MonadsSandbox/ErrorMonad.{runError, throwError};
    traverseM(x -> {println(x); if (x < 10) point(x) else throwError("too big")}, List.range(0, 10000001)) |> runError    