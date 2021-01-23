
/// This is probably the "best" definition of `count` for Option.
/// We only do one case analysis and `List.repeat` is tail recursive
def countDirect(ntimes: Int32, mx: Option[a]): Option[List[a]] = match mx { 
    case None => None
    case Some(x1) => Some(List.repeat(x1, ntimes))
}



def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    let xs1: Option[List[String]] = countDirect(10, Some("a"): Option[String]);
    Console.printLine("xs1: ${xs1}");
    let xs2: Option[List[String]] = countDirect(10, None: Option[String]);
    Console.printLine("xs2: ${xs2}");
    0
