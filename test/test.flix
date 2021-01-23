use Applicative.{point};
use Alternative.{tryWithDefault};
use MonadLib/Transformers.{Id};
use MonadLib/Transformers.Id.{Id};  /// temp
use MonadLib/Transformers.{runId};


def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    let action1: Id[Int32] = point(0);
    let ans: Int32 = runId(action1);
    Console.printLine("ans: ${ans}");
    let x1: Option[String] = tryWithDefault(None, "world");
    Console.printLine("x1: ${x1}");
    let x2: Option[String] = tryWithDefault(Some("Hello"), "world");
    Console.printLine("x2: ${x2}");
    ans
