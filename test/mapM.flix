use Applicative.{consA, point};

/// Short circuits but not tail recursive
def mapMv1(f: a -> Option[b], xs: List[a]): Option[List[b]] = match xs { 
    case x :: rs => match f(x) { 
        case Some(x1) => ocons(x1, mapMv1(f, rs))
        case None => None
    }
    case Nil => Some(Nil)
}

def ocons(x: a, xs: Option[List[a]]): Option[List[a]] = match xs { 
    case Some(xs1) => Some(x :: xs1)
    case None => None
}

def mapMtr[a, b, m :# Type -> Type : Monad](f: a -> m[b], xs: List[a]): m[List[b]] =
    mapMtrHelper(f, xs, point(Nil)) |> Functor.map(List.reverse)

def mapMtrHelper[a, b, m :# Type -> Type : Monad](f: a -> m[b], xs: List[a], ac: m[List[b]]): m[List[b]] = match xs { 
    case x :: rs => mapMtrHelper(f, rs, consA(f(x), ac)) 
    case Nil => ac
}

/// Just need Applicative...
def mapMcps[a, b, m :# Type -> Type : Applicative](f: a -> m[b], xs: List[a]): m[List[b]] =
    mapMcpsHelper(f, xs, mks -> mks)

def mapMcpsHelper[a, b, m :# Type -> Type : Applicative](f: a -> m[b], xs: List[a], k: m[List[b]] -> m[List[b]]): m[List[b]] = match xs { 
    case x :: rs => mapMcpsHelper(f, rs, mks -> k(consA(f(x), mks)))
    case Nil => k(point(Nil))
}


def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running");
    let xs1: Option[List[Int32]] = mapMv1(x -> if (x > 2) None else Some(x), 1 :: 2 :: Nil);
    Console.printLine("xs1: ${xs1}");
    let xs2: Option[List[Int32]] = mapMtr(x -> if (x > 10) None else Some(x), 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
    Console.printLine("xs2: ${xs2}"); 
    let xs3: Option[List[Int32]] = mapMcps(x -> if (x > 10) None else Some(x), 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
    Console.printLine("xs3: ${xs3}"); 
    match mapMcps(x -> if (x < 0) None else Some(x), List.range(1, 15000)) { 
        case Some(ys1) => Console.printLine("CPS good, len = ${List.length(ys1)}")
        case None => Console.printLine("CPS bad should not have seen None")
    };
    match mapMcps(x -> if (x > 10) None else Some(x), List.range(1, 15000)) { 
        case Some(ys2) => Console.printLine("CPS bad, not expecting a list [len = ${List.length(ys2)}]")
        case None => Console.printLine("CPS good - expecting None")
    };
    0


