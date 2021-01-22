use Applicative.{point};
use MonadLib/Transformers.{Id};
use MonadLib/Transformers.Id.{Id};  /// temp
use MonadLib/Transformers.{runId};


def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    let action1: Id[Int32] = point(0);
    let ans: Int32 = runId(action1);
    Console.printLine("ans: ${ans}");
    ans
