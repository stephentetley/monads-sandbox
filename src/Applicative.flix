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


class Applicative[m :# Type -> Type] extends Functor[m] {
    
    pub def point(x: a): m[a] 

    pub def ap(f: m[a -> b & ef] , x: m[a]): m[b] & ef

}


instance Applicative[List] {

    def point(x: a): List[a] = x :: Nil
    
    def ap(f: List[a -> b & ef], x: List[a]): List[b] & ef = List.zipWith((f1, a) -> f1(a), f, x)
}

instance Applicative[Option] {

    def point(x: a): Option[a] = Some(x)
    
    def ap(f: Option[a -> b & ef], x: Option[a]): Option[b] & ef = 
        match f { 
            case None => None
            case Some(f1) => Option.map(f1, x)
        }
}


namespace Applicative {

    /// For Functor ...
    pub def ignore[a, m :# Type -> Type : Functor](mx: m[a]): m[Unit] =
        Functor.map(constant(), mx)


    // Haskell's <$>
    pub def <&>[a, b, ef, m :# Type -> Type : Functor](f: a -> b & ef, mx: m[a]): m[b] & ef =
        Functor.map(f, mx)

    pub def <&[a, b, m :# Type -> Type : Functor](x: a, mx: m[b]): m[a] =
        Functor.map(_ -> x, mx)

    /// Applicative functions...


    ///  Applicative cons.
    pub def consA[a, m :# Type -> Type : Applicative](f: m[a], fs: m[List[a]]): m[List[a]] =
        (((x, xs) -> x :: xs) <&> f) <*> fs

    // Haskell's <*> (i.e `ap`)
    pub def <*>[a, b, ef, m :# Type -> Type : Applicative](f: m[a -> b & ef], mx: m[a]): m[b] & ef =
        Applicative.ap(f, mx)

    pub def <**>[a, b, ef, m :# Type -> Type : Applicative](mx: m[a], f: m[a -> b & ef]): m[b] & ef =
        Applicative.ap(f, mx)

    pub def seqLeft[a, b, m :# Type -> Type : Applicative](mx: m[a], my: m[b]): m[a] = 
        (((x1, _) -> x1) <&> mx) <*> my

    pub def seqRight[a, b, m :# Type -> Type : Applicative](mx: m[a], my: m[b]): m[b] = 
        (((_, x2) -> x2) <&> mx) <*> my


    /// Alias for `seqLeft`
    pub def <*[a, b, m :# Type -> Type : Applicative](mx: m[a], my: m[b]): m[a] = seqLeft(mx, my)

    /// Alias for `seqRight`
    pub def *>[a, b, m :# Type -> Type : Applicative](mx: m[a], my: m[b]): m[b] = seqRight(mx, my)


    pub def between[a, o, c, m :# Type -> Type : Applicative](start: m[o], end: m[c], mx: m[a]): m[a] = 
        seqLeft((seqRight(start, mx)), end)


    pub def whenTrue[a, m :# Type -> Type : Applicative](test: Bool, ma: m[a]): m[Option[a]] = 
        match test { 
            case true => Functor.map(Some, ma)
            case false => point(None)
        }

    pub def whenFalse[a, m :# Type -> Type : Applicative](test: Bool, ma: m[a]): m[Option[a]] = 
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

    pub def count[a, m :# Type -> Type : Applicative](ntimes: Int32, mx: m[a]): m[List[a]] = 
        countHelper(ntimes, mx, point(Nil))

    def countHelper[a, m :# Type -> Type : Applicative](ntimes: Int32, mx: m[a], ac: m[List[a]]): m[List[a]] = 
        if (ntimes <= 0)
            ac
        else 
            countHelper(ntimes - 1, mx, consA(mx, ac))

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

    
    
    pub def liftA2[t1, t2, t3, ef, m :# Type -> Type : Applicative](f: (t1, t2) -> t3 & ef, a1: m[t1], a2: m[t2]): m[t3] & ef = 
        (((x1, x2) -> f(x1, x2)) <&> a1) <*> a2

    pub def liftA3[t1, t2, t3, t4, ef, m :# Type -> Type : Applicative](f: (t1, t2, t3) -> t4 & ef, a1: m[t1], a2: m[t2], a3: m[t3]): m[t4] & ef = 
        liftA2(f, a1, a2) <*> a3

    pub def liftA4[t1, t2, t3, t4, t5, ef, m :# Type -> Type : Applicative](f: (t1, t2, t3, t4) -> t5 & ef, a1: m[t1], a2: m[t2], a3: m[t3], a4: m[t4]): m[t5] & ef = 
        liftA3(f, a1, a2, a3) <*> a4

    pub def liftA5[t1, t2, t3, t4, t5, t6, ef, m :# Type -> Type : Applicative](f: (t1, t2, t3, t4, t5) -> t6 & ef, a1: m[t1], a2: m[t2], a3: m[t3], a4: m[t4], a5: m[t5]): m[t6] & ef = 
        liftA4(f, a1, a2, a3, a4) <*> a5

    pub def liftA6[t1, t2, t3, t4, t5, t6, t7, ef, m :# Type -> Type : Applicative](f: (t1, t2, t3, t4, t5, t6) -> t7 & ef, a1: m[t1], a2: m[t2], a3: m[t3], a4: m[t4], a5: m[t5], a6: m[t6]): m[t7] & ef = 
        liftA5(f, a1, a2, a3, a4, a5) <*> a6

    pub def liftA7[t1, t2, t3, t4, t5, t6, t7, t8, ef, m :# Type -> Type : Applicative](f: (t1, t2, t3, t4, t5, t6, t7) -> t8 & ef, a1: m[t1], a2: m[t2], a3: m[t3], a4: m[t4], a5: m[t5], a6: m[t6], a7: m[t7]): m[t8] & ef = 
        liftA6(f, a1, a2, a3, a4, a5, a6) <*> a7

    pub def liftA8[t1, t2, t3, t4, t5, t6, t7, t8, t9, ef, m :# Type -> Type : Applicative](f: (t1, t2, t3, t4, t5, t6, t7, t8) -> t9 & ef, a1: m[t1], a2: m[t2], a3: m[t3], a4: m[t4], a5: m[t5], a6: m[t6], a7: m[t7], a8: m[t8]): m[t9] & ef = 
        liftA7(f, a1, a2, a3, a4, a5, a6, a7) <*> a8

    pub def liftA9[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, ef, m :# Type -> Type : Applicative](f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> t10 & ef, a1: m[t1], a2: m[t2], a3: m[t3], a4: m[t4], a5: m[t5], a6: m[t6], a7: m[t7], a8: m[t8], a9: m[t9]): m[t10] & ef = 
        liftA8(f, a1, a2, a3, a4, a5, a6, a7, a8) <*> a9

    pub def liftA10[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, ef, m :# Type -> Type : Applicative](f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> t11 & ef, a1: m[t1], a2: m[t2], a3: m[t3], a4: m[t4], a5: m[t5], a6: m[t6], a7: m[t7], a8: m[t8], a9: m[t9], a10: m[t10]): m[t11] & ef = 
        liftA9(f, a1, a2, a3, a4, a5, a6, a7, a8, a9) <*> a10


}