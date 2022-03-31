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


namespace FoldableExtras {

    use Applicative.{*>};


    /// PR #2932 (with polymorphic effects)
    ///
    /// A monadic version of `foldLeft`.
    /// 
    /// Applies the monadic `f` to a start value `s` and all elements in `t` going 
    /// from left to right.
    ///
    pub def foldLeftM(f: (b, a) -> m[b], s: b, t: t[a]): m[b] with Foldable[t], Monad[m] =
        let f1 = (x, k) -> z -> Monad.flatMap(k, f(z, x));
        s |> Foldable.foldRight(f1, Applicative.point, t)

    /// PR #2932 (with polymorphic effects)
    ///
    /// A monadic version of `foldRight`.
    /// 
    /// Applies the monadic `f` to a start value `s` and all elements in `t` going 
    /// from right to left.
    ///
    pub def foldRightM(f: (a, b) -> m[b], s: b, t: t[a]): m[b] with Foldable[t], Monad[m] =
        let f1 = (k, x) -> z -> Monad.flatMap(k, f(x, z));
        s |> Foldable.foldLeft(f1, Applicative.point, t)

    /// PR #2932 (with polymorphic effects)
    ///
    /// A monadic version of `foreach`.
    /// 
    /// Apply `f` to every value in `t`. `f` is applied for its monadic effect, 
    /// the answer it produces is discarded.
    ///
    pub def foreachM(f: a -> m[b], t: t[a]): m[Unit] with Foldable[t], Monad[m] =
        foldLeftM((acc, a) -> f(a) *> Applicative.point(acc), (), t)




    pub def sequenceX(t: t[m[a]]): m[Unit] with Applicative[m], Foldable[t] =
        Foldable.foldLeft((mb, ma) -> ma *> mb, Applicative.point(()), t)

    pub def traverseX(f: a -> m[b] & ef, t: t[a]): m[Unit] & ef with Foldable[t], Applicative[m] =
        Foldable.foldLeft((mb, a) -> f(a) *> mb, Applicative.point(()), t)

    pub def forX(t: t[a], f: a -> m[b] & ef): m[Unit] & ef with Foldable[t], Applicative[m] = traverseX(f, t)


    pub def concat(t: t[List[a]]): List[a] with Foldable[t] =
        Foldable.foldLeft((ac, xs) -> Chain.append(ac, List.toChain(xs)), Chain.empty(), t) |> Chain.toList

    pub def concatMap(f: a -> List[b], t: t[a]): List[b] with Foldable[t] =
        Foldable.foldLeft((ac, x) -> Chain.append(ac, List.toChain(f(x))), Chain.empty(), t) |> Chain.toList

    /// `String.unlines` generalized to any Foldable `t[String]`.
    pub def unlines(t: t[String]): String with Foldable[t] = unlinesHelper(t) as & Pure

    def unlinesHelper(t: t[String]): String & Impure with Foldable[t] =
        let sb = StringBuilder.new();
        Foldable.foldLeft((_, x) -> StringBuilder.appendLine!(x, sb), (), t);
        StringBuilder.toString(sb)

    /// `String.unwords` generalized to any Foldable `t[String]`.
    pub def unwords(t: t[String]): String with Foldable[t] = unwordsHelper(t) as & Pure

    def unwordsHelper(t: t[String]): String & Impure with Foldable[t] =
        let sb = StringBuilder.new();
        let step = (n, x) ->
            if (n < 1)
                {StringBuilder.appendString!(x, sb); n + 1}
            else
                {StringBuilder.append!(' ', sb); StringBuilder.appendString!(x, sb); n + 1};
        Foldable.foldLeft(step, 0, t);
        StringBuilder.toString(sb)

}
