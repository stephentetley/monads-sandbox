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

pub class Unfoldable[t: Type -> Type] {

    pub def unfold(f: b -> Option[(a, b)] & ef, start: b): t[a] & ef

    pub def replicate(n: Int32, v:a): t[a] = 
        let step = i -> if (i <= 0) None else Some((v, i - 1));
        Unfoldable.unfold(step, n)

    pub def unfoldWithIter(next: Unit -> Option[a] & ef): t[a] & ef = 
        let next1 = s -> match next(s) {            
            case None => None
            case Some(a) => Some(a, ())
        };
        Unfoldable.unfold(next1, ())


    pub def singleton(x: a): t[a] = 
        Unfoldable.replicate(1, x)        


    pub def fromOption(x: Option[a]): t[a] = 
        Unfoldable.unfold(b -> Functor.map(c -> (c, None), b), x)

}

instance Unfoldable[List] {
    pub def unfold(f: b -> Option[(a, b)] & ef, start: b): List[a] & ef = 
        List.unfold(f, start)

    pub override def singleton(x: a): List[a] = x :: Nil
}

instance Unfoldable[Chain] {
    pub def unfold(f: b -> Option[(a, b)] & ef, start: b): Chain[a] & ef =
        def loop(st, k) = match f(st) {
            case None => k(Chain.empty())
            case Some((a, st1)) => loop(st1, ks -> k(Chain.cons(a, ks)))
        };
        loop(start, k -> k)

    pub override def singleton(x: a): Chain[a] = Chain.singleton(x)

}

instance Unfoldable[Option] {
    pub def unfold(f: b -> Option[(a, b)] & ef, start: b): Option[a] & ef = 
        Option.map(fst, f(start))
   
}


namespace Unfoldable {


    pub def replicateA(n: Int32, x: m[a]): m[t[a]] with Applicative[m], Traversable[t], Unfoldable[t] =
        Traversable.sequence(Unfoldable.replicate(n, x)) 

    pub def none(): f[a] with Unfoldable[f] = 
        Unfoldable.unfold(constant(None), ())


}