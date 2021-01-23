use Applicative.{point, count, filterA};
use Alternative.{optionalWithDefault};
use MonadLib/Transformers.{Id};
use MonadLib/Transformers.Id.{Id};  /// temp
use MonadLib/Transformers.{runId};


def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    let action1: Id[Int32] = point(0);
    let ans: Int32 = runId(action1);
    Console.printLine("ans: ${ans}");
    let x1: Option[String] = optionalWithDefault(None, "world");
    Console.printLine("x1: ${x1}");
    let x2: Option[String] = optionalWithDefault(Some("Hello"), "world");
    Console.printLine("x2: ${x2}");
    let xs1: Option[List[String]] = count(10, point("a"): Option[String]);
    Console.printLine("xs1: ${xs1}");
    let xs2: Option[List[String]] = count(10, None: Option[String]);
    Console.printLine("xs2: ${xs2}");
    let xs3: Option[List[Int32]] = filterA(x -> if (x < 5) Some(true) else Some(false), 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
    Console.printLine("xs3: ${xs3}");
    ans
