use Applicative.{consA, point};

/// This is probably the "best" definition of `count` for Option.
/// We only do one case analysis and `List.repeat` is tail recursive
def countDirect(ntimes: Int32, mx: Option[a]): Option[List[a]] = match mx { 
    case None => None
    case Some(x1) => Some(List.repeat(x1, ntimes))
}


def countAc[a, m :# Type -> Type : Applicative](ntimes: Int32, mx: m[a]): m[List[a]] = 
    countAcHelper(ntimes, mx, point(Nil))

def countAcHelper[a, m :# Type -> Type : Applicative](ntimes: Int32, mx: m[a], ac: m[List[a]]): m[List[a]] = 
    if (ntimes <= 0)
        ac
    else 
        countAcHelper(ntimes - 1, mx, consA(mx, ac))

/// count is replicateM

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    let xs1: Option[List[String]] = countDirect(10, Some("a"): Option[String]);
    Console.printLine("xs1: ${xs1}");
    let xs2: Option[List[String]] = countDirect(10, None: Option[String]);
    Console.printLine("xs2: ${xs2}");
    let xs3: Option[List[String]] = countAc(10, point("aaa"));
    Console.printLine("xs3: ${xs3}");
    match countAc(15000, point("aaa")) { 
        case None => Console.printLine("What?")
        case Some(ys) => Console.printLine("Len: ${List.length(ys)}")
    };
    0
