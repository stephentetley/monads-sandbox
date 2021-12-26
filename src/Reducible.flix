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



pub lawless class Reducible[t : Type -> Type] with Foldable[t] {


    pub def reduceLeftTo(f: a -> b & ef1, comb: (b, a) -> b & ef2, t: t[a]): b & (ef1 and ef2)

    pub def reduceRightTo(f: a -> b & ef1, comb: (a, b) -> b & ef2, t: t[a]): b & (ef1 and ef2)
        
    pub def reduceLeft(f: (a, a) -> a & ef, t: t[a]): a & ef = 
        Reducible.reduceLeftTo(identity, f, t)

    pub def reduceRight(f: (a, a) -> a & ef, t: t[a]): a & ef =
        Reducible.reduceRightTo(identity, f, t)

    pub def reduceMap(f: a -> b, t: t[a]): b with SemiGroup[b] =
        Reducible.reduceLeftTo(f, (b, a) -> SemiGroup.combine(b, f(a)), t)

    pub def minimum(t: t[a]): a with Order[a] =
        Reducible.reduceLeft(Order.min, t)

    pub def minimumBy(cmp: (a, a) -> Comparison, t: t[a]): a with Order[a] =
        Reducible.reduceLeft(Order.minBy(cmp), t)

    pub def maximum(t: t[a]): a with Order[a] =
        Reducible.reduceLeft(Order.max, t)

    pub def maximumBy(cmp: (a, a) -> Comparison, t: t[a]): a with Order[a] =
        Reducible.reduceLeft(Order.maxBy(cmp), t)

    ///
    /// Returns `t` as an non-empty list.
    ///
    pub def toNel(t: t[a]): Nel[a] =
        Reducible.reduceRightTo(Nel.singleton, Nel.cons, t)

}

instance Reducible[Nel] {

    pub def reduceLeftTo(f: a -> b & ef1, comb: (b, a) -> b & ef2, l: Nel[a]): b & (ef1 and ef2) = match l {
        case Nel(h, tl) => List.foldLeft(comb, f(h), tl)
    }

    /// Must test...
    pub def reduceRightTo(f: a -> b & ef1, comb: (a, b) -> b & ef2, l: Nel[a]): b & (ef1 and ef2) = 
        def loop(x, tl, cont) = match tl {
            case Nil => cont(f(x))
            case x1 :: rs => loop(x1, rs, b1 -> cont(comb(x, b1)))
        };
        match l {
            case Nel(h, tl) => loop(h, tl, acc -> acc as & (ef1 and ef2))
        }

    pub override def reduceLeft(f: (a, a) -> a & ef, l: Nel[a]): a & ef = Nel.reduceLeft(f, l)

    pub override def reduceRight(f: (a, a) -> a & ef, l: Nel[a]): a & ef = Nel.reduceRight(f, l)

    pub override def toNel(t: Nel[a]): Nel[a] = t
}

namespace Reducible {
    
    pub def reduce(t: t[a]): a with SemiGroup[a], Reducible[t] = 
        reduceLeft(SemiGroup.combine, t)
}
