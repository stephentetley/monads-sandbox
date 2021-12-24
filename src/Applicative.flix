/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */




namespace ApplicativeTemp {

    use Applicative.{point};

    /// For Functor ...
    pub def ignore(mx: m[a]): m[Unit] with Functor[m] =
        Functor.map(constant(), mx)


    // Haskell's <$>
    pub def <&>(f: a -> b & ef, mx: m[a]): m[b] & ef with Functor[m] =
        Functor.map(f, mx)

    pub def <&(x: a, mx: m[b]): m[a] with Functor[m] =
        Functor.map(_ -> x, mx)

    /// Applicative functions...


    ///  Applicative cons.
    pub def consA(f: m[a], fs: m[List[a]]): m[List[a]] with Applicative[m] =
        (((x, xs) -> x :: xs) <&> f) <*> fs

    // Haskell's <*> (i.e `ap`)
    pub def <*>(f: m[a -> b & ef], mx: m[a]): m[b] & ef with Applicative[m] =
        Applicative.ap(f, mx)

    pub def <**>(mx: m[a], f: m[a -> b & ef]): m[b] & ef with Applicative[m] =
        Applicative.ap(f, mx)

    pub def seqLeft(mx: m[a], my: m[b]): m[a] with Applicative[m] = 
        (((x1, _) -> x1) <&> mx) <*> my

    pub def seqRight(mx: m[a], my: m[b]): m[b] with Applicative[m] = 
        (((_, x2) -> x2) <&> mx) <*> my


    /// Alias for `seqLeft`
    pub def <*(mx: m[a], my: m[b]): m[a] with Applicative[m] = seqLeft(mx, my)

    /// Alias for `seqRight`
    pub def *>(mx: m[a], my: m[b]): m[b] with Applicative[m] = seqRight(mx, my)


    pub def between(start: m[o], end: m[c], mx: m[a]): m[a] with Applicative[m] = 
        seqLeft((seqRight(start, mx)), end)


    pub def whenTrue(test: Bool, ma: m[a]): m[Option[a]] with Applicative[m] = 
        match test { 
            case true => Functor.map(Some, ma)
            case false => point(None)
        }

    pub def whenFalse(test: Bool, ma: m[a]): m[Option[a]] with Applicative[m] = 
        match test { 
            case true => point(None)
            case false => Functor.map(Some, ma)
        }

    /// pub def whenSome[a, b, m :# Type -> Type : Applicative](x: Option[a], f: a -> m[b]): m[Option[b]] = 
    ///     match x { 
    ///         case Some(a) => Functor.map(Some, f(a))
    ///         case None => point(None)
    ///     }

    /// pub def whenNone[a, b, m :# Type -> Type : Applicative](x: Option[a], ma: m[b]): m[Option[b]] = 
    ///     match x { 
    ///         case Some(_) => point(None)
    ///         case None => Functor.map(Some, ma)
    ///     }

    pub def replicateA(ntimes: Int32, mx: m[a]): m[List[a]] with Applicative[m] = 
        replicateAHelper(ntimes, mx, point(Nil))

    def replicateAHelper(ntimes: Int32, mx: m[a], ac: m[List[a]]): m[List[a]] with Applicative[m] = 
        if (ntimes <= 0)
            ac
        else 
            replicateAHelper(ntimes - 1, mx, consA(mx, ac))

    pub def replicateX(ntimes: Int32, mx: m[a]): m[Unit] with Applicative[m] = 
        replicateXHelper(ntimes, mx, point(()))

    def replicateXHelper(ntimes: Int32, mx: m[a], ac: m[Unit]): m[Unit] with Applicative[m] = 
        if (ntimes <= 0)
            ac
        else 
            replicateXHelper(ntimes - 1, mx, ignore(mx))

/*

    /// aka Haskell's mapM but it just needs Applicative...
    /// Caution - this does not short-circuit if `m` supports failure
    pub def mapA[a, b, m :# Type -> Type : Applicative](f: a -> m[b], xs: List[a]): m[List[b]] =
        mapAHelper(f, xs, mks -> mks)

    def mapAHelper[a, b, m :# Type -> Type : Applicative](f: a -> m[b], xs: List[a], k: m[List[b]] -> m[List[b]]): m[List[b]] = match xs { 
        case x :: rs => mapAHelper(f, rs, mks -> k(consA(f(x), mks)))
        case Nil => k(point(Nil))
    }

    pub def mapX[a, b, m :# Type -> Type : Applicative](f: a -> m[b], xs: List[a]): m[Unit] = 
        mapXHelper(f, xs, point(()))

    def mapXHelper[a, b, m :# Type -> Type : Applicative](f: a -> m[b], xs: List[a], ac: m[Unit]): m[Unit] = match xs { 
        case Nil => ac
        case x :: rs => mapXHelper(f, rs, ignore(f(x)))
    }

    pub def forA[a, b, m :# Type -> Type : Applicative](xs: List[a], f: a -> m[b]): m[List[b]] =
        mapA(f, xs)

    pub def forX[a, b, m :# Type -> Type : Applicative](xs: List[a], f: a -> m[b]): m[Unit] = 
        mapX(f, xs)


    pub def sequenceA[a, m :# Type -> Type : Applicative](xs: List[m[a]]): m[List[a]] = 
        sequenceAHelper(xs, mks -> mks)

    def sequenceAHelper[a, m :# Type -> Type : Applicative](xs: List[m[a]], k: m[List[a]] -> m[List[a]]): m[List[a]] = match xs {
        case mx :: rs => sequenceAHelper(rs, mks -> k(consA(mx, mks)))
        case Nil => k(point(Nil))
    }

    pub def sequenceX[a, m :# Type -> Type : Applicative](xs: List[m[a]]): m[Unit] = 
        sequenceXHelper(xs, point(()))

    def sequenceXHelper[a, m :# Type -> Type : Applicative](xs: List[m[a]], ac: m[Unit]): m[Unit] = match xs {
        case mx :: rs => sequenceXHelper(rs, ignore(mx))
        case Nil => ac
    }

    pub def filterA[a, m :# Type -> Type : Applicative](f: a -> m[Bool], xs: List[a]): m[List[a]] = 
        filterAHelper(f, xs, mks -> mks)

    def filterAHelper[a, m :# Type -> Type : Applicative](f: a -> m[Bool], xs: List[a], k: m[List[a]] -> m[List[a]]): m[List[a]] = match xs {
        case x :: rs => filterAHelper(f, rs, mks -> k(whenConsA(f, x, mks)))
        case Nil => k(point(Nil))
    }
    
    def whenConsA[a, m :# Type -> Type : Applicative](f: a -> m[Bool], x: a, fs: m[List[a]]): m[List[a]] =
        (((test, xs) -> if (test) (x :: xs) else xs) <&> f(x)) <*> fs


    pub def mapAndUnzipA[a, b, c, m :# Type -> Type : Monad](f: a -> m[(b, c)], xs: List[a]): m[(List[b], List[c])] = 
        mapAndUnzipAHelper(f, xs, mks -> mks)

    def mapAndUnzipAHelper[a, b, c, m :# Type -> Type : Monad](f: a -> m[(b, c)], xs: List[a], k: m[(List[b], List[c])] -> m[(List[b], List[c])]): m[(List[b], List[c])] = match xs {
        case x :: rs1 => mapAndUnzipAHelper(f, rs1, mks -> k(doubleConsA(f(x), mks)))
        case Nil => k(point((Nil, Nil)))
    }

    def doubleConsA[a, b, m :# Type -> Type : Applicative](mx: m[(a, b)], rs: m[(List[a], List[b])]): m[(List[a], List[b])] =
        (((a, b) -> {let (x, y) = a; let (xs, ys) = b; (x :: xs, y :: ys)}) <&> mx) <*> rs


    pub def zipWithA[a, b, c, m :# Type -> Type : Applicative](f: (a, b) -> m[c], xs: List[a], ys: List[b]): m[List[c]] = 
        zipWithAHelper(f, xs, ys, mks -> mks)

    def zipWithAHelper[a, b, c, m :# Type -> Type : Applicative](f: (a, b) -> m[c], xs: List[a], ys: List[b], k: m[List[c]] -> m[List[c]]): m[List[c]] = match (xs, ys) {
        case (x :: rs1, y :: rs2) => zipWithAHelper(f, rs1, rs2, mks -> k(consA(f(x, y), mks)))
        case (_, _) => k(point(Nil))
    }


    pub def zipWithX[a, b, c, m :# Type -> Type : Applicative](f: (a, b) -> m[c], xs: List[a], ys: List[b]): m[Unit] = 
        zipWithXHelper(f, xs, ys, point(()))

    def zipWithXHelper[a, b, c, m :# Type -> Type : Applicative](f: (a, b) -> m[c], xs: List[a], ys: List[b], ac: m[Unit]): m[Unit] = match (xs, ys) {
        case (x :: rs1, y :: rs2) => zipWithXHelper(f, rs1, rs2, ignore(f(x, y)))
        case (_, _) => ac
    }


*/

}