use Applicative.{point, replicateA, filterA, mapAndUnzipA};
use Alternative.{optionalWithDefault};
use MonadLib/Monads.{Id};
use MonadLib/Monads.Id.{Id};  /// temp
use MonadLib/Monads.{runId};


def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    let action1: Id[Int32] = point(0);
    let ans: Int32 = runId(action1);
    Console.printLine("ans: ${ans}");
    let x1: Option[String] = optionalWithDefault(None, "world");
    Console.printLine("x1: ${x1}");
    let x2: Option[String] = optionalWithDefault(Some("Hello"), "world");
    Console.printLine("x2: ${x2}");
    let xs1: Option[List[String]] = replicateA(10, point("a"): Option[String]);
    Console.printLine("xs1: ${xs1}");
    let xs2: Option[List[String]] = replicateA(10, None: Option[String]);
    Console.printLine("xs2: ${xs2}");
    let xs3: Option[List[Int32]] = filterA(x -> if (x < 5) Some(true) else Some(false), 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
    Console.printLine("xs3: ${xs3}");
    match mapAndUnzipA(a -> Some("''${a}''", a + 1), List.range(1, 11)) { 
        case Some((zs1, zs2)) => Console.printLine("mapAndUnzipA: ${zs1}; ${zs2}")
        case None => Console.printLine("mapAndUnzipA: Wrong")
    };
    ans
