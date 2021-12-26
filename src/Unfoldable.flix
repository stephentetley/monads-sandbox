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

pub lawless class Unfoldable[t: Type -> Type] {

    pub def unfold(f: b -> Option[(a, b)] & ef, b0: b): t[a] & ef

}

instance Unfoldable[List] {
    pub def unfold(f: b -> Option[(a, b)] & ef, b0: b): List[a] & ef = 
        List.unfold(f, b0)
}

instance Unfoldable[Chain] {
    pub def unfold(f: b -> Option[(a, b)] & ef, b0: b): Chain[a] & ef =
        def loop(st, k) = match f(st) {
            case None => k(Chain.empty())
            case Some((a, st1)) => loop(st1, ks -> k(Chain.cons(a, ks)))
        };
        loop(b0, k -> k)
}

namespace Unfoldable {

    pub def replicate(n: Int32, v:a): t[a] with Unfoldable[t] = 
        let step = i -> if (i <= 0) None else Some((v, i - 1));
        Unfoldable.unfold(step, n)

    /// pub def replicateA(n: Int32, v: m[a]): m[t[a]] with Applicative[m], Traversable[t], Unfoldable[t] =
    ///     Traversable.sequence(replicate(n, v)) 

    pub def none(): f[a] with Unfoldable[f] = 
        Unfoldable.unfold(constant(None), ())

    pub def singleton(x: a): f[a] with Unfoldable[f] = 
        replicate(1, x)

    pub def fromOption(x: Option[a]): f[a] with Unfoldable[f] = 
        Unfoldable.unfold(b -> Functor.map(c -> (c, None), b), x)

}