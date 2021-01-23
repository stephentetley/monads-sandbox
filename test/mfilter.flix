use Applicative.{point, <&>, <*>};
use Alternative.{zero};
use Monad.{bind};

def mfilterMonad[a, m :# Type -> Type : Monad : Alternative](f: a -> Bool, mx: m[a]): m[a] = 
    bind(mx, x -> if (f(x)) point(x) else zero())

/// An mfilter function is definable with just Alternative but gives the "wrong" answer
def mfilterAlt[a, m :# Type -> Type : Alternative](f: a -> Bool, mx: m[a]): m[a] = 
    (((x, y) -> if(f(x)) x else y) <&> mx) <*> zero()
    

/// def even(x: Int32): Bool = x % 2 == 0

def odd(x: Int32): Bool = x % 2 != 0

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    let ans1 = mfilterMonad(odd, Some(5));
    Console.printLine("ans1: ${ans1}");
    let ans2 = mfilterMonad(odd, Some(4));
    Console.printLine("ans2: ${ans2}");
    let ans3 = mfilterAlt(odd, Some(5));
    Console.printLine("ans3: ${ans3}");
    let ans4 = mfilterAlt(odd, Some(4));
    Console.printLine("ans4: ${ans4}");
    0
