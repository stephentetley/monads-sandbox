
def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    MonadLib/Monad/ContState.runContState(Applicative.point("hello"), 0) |> println;
    0
