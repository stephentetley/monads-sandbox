/*
 * Copyright 2020 Stephen Tetley
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

// Note - not in the Data namespace.

enum Either[a, b] {
    case Left(a),
    case Right(b)
}

instance Functor[Either[e]] {
    pub def map(f: a -> b & ef, x: Either[e, a]): Either[e, b] & ef = Either.mapRight(f, x)
}

instance Applicative[Either[e]] {
    pub def point(x: a): Either[e, a] = Right(x)

    pub def ap(mf: Either[e, a -> b & ef], mx: Either[e, a]): Either[e, b] & ef = match mf {
        case Right(f) => match mx {
            case Right(x) => Right(f(x))
            case Left(a1) => Left(a1)
        }
        case Left(a) => Left(a)
    }
}

instance Monad[Either[e]] {
    pub def flatMap(k: a -> Either[e, b] & ef, ma: Either[e, a]): Either[e, b] & ef = match ma {
        case Right(a) => k(a)
        case Left(e) => Left(e) as & ef
    }
}

/*

/// Waiting for PR #2893...

instance Traversable[Either[e]] {
    pub def traverse(f: a -> m[b] & ef, t: Either[e, a]): m[Either[e, b]] & ef with Applicative[m] =
        use Applicative.{point};
        match t {
            case Left(e) => point(Left(e))
            case Right(a) => Functor.map(Right, f(a))
        }

    pub override def sequence(t: Either[e, m[a]]): m[Either[e, a]] with Applicative[m] =
        use Applicative.{point};
        match t {
            case Left(e) => point(Left(e))
            case Right(ma) => Functor.map(Right, ma)
        }
}
*/

namespace Either {


    ///
    /// Returns `true` iff `x` is `Left(_)`.
    ///
    pub def isLeft(x: Either[a, b]): Bool = match x {
        case Left(_) => true
        case Right(_) => false
    }

    ///
    /// Returns `true` iff `r` is `Right(_)`.
    ///
    pub def isRight(x: Either[a, b]): Bool = match x {
        case Left(_) => false
        case Right(_) => true
    }


    pub def either(f: a -> c, g: b -> c, x: Either[a, b]): c = match x {
        case Left(a) => f(a)
        case Right(b) => g(b)
    }

    pub def mapLeft(f: a -> c & ef, x: Either[a, b]): Either[c, b] & ef = match x {
        case Left(a) => Left(f(a))
        case Right(b) => Right(b) as & ef
    }

    pub def mapRight(g: b -> c & ef, x: Either[a, b]): Either[a, c] & ef = match x {
        case Left(a) => Left(a) as & ef
        case Right(b) => Right(g(b))
    }

    pub def fromLeft(d: a, x: Either[a, b]): a = match x {
        case Left(a) => a
        case Right(_) => d
    }

    pub def fromRight(d: b, x: Either[a ,b]): b = match x {
        case Left(_) => d
        case Right(b) => b
    }

    pub def partition(xs: List[Either[a,b]]): (List[a], List[b]) =
        partitionHelper(xs, ls -> ls, rs -> rs)

    def partitionHelper(xs: List[Either[a,b]], k1: List[a] -> List[a], k2: List[b] -> List[b]): (List[a], List[b]) =
        match xs {
            case Nil => (k1(Nil), k2(Nil))
            case x :: rs => match x {
                case Left(a) => partitionHelper(rs, ys -> k1(a :: ys), k2)
                case Right(b) => partitionHelper(rs, k1, zs -> k2(b :: zs))
            }
        }

    pub def lefts(xs: List[Either[a,b]]): List[a] =
        leftsHelper(xs, zs -> zs)

    def leftsHelper(xs: List[Either[a,b]], k: List[a] -> List[a]): List[a] = match xs {
        case Nil => k(Nil)
        case x :: rs => match x {
            case Left(a) => leftsHelper(rs, zs -> k(a :: zs))
            case Right(_) => leftsHelper(rs, k)
        }
    }

    pub def rights(xs: List[Either[a,b]]): List[b] =
        rightsHelper(xs, zs -> zs)

    def rightsHelper(xs: List[Either[a,b]], k: List[b] -> List[b]): List[b] = match xs {
        case Nil => k(Nil)
        case x :: rs => match x {
            case Left(_) => rightsHelper(rs, k)
            case Right(b) => rightsHelper(rs, zs -> k(b :: zs))
        }
    }


}
