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

/// This embodies parsing (and possibly only parsing) where you expect to encounter tokens
/// representing different things e.g values, separators.
///
/// This is not a natural fit for regex matching - in regex matching, all matched tokens are
/// same "thing" (give or take a bit of hand-waving about the pipe operator). `sepEndBy` doesn't
/// make much sense if your regex matcher is identifying a stream of numbers.
///



pub lawless class Many[m : Type -> Type] with Alternative[m] {

    ///
    /// Must be stack-safe for derived combinators to work safely.
    ///
    pub def manyWith(combine: (b, a) -> b, acc: b, ma: m[a]): m[b]

    ///
    /// Must be stack-safe for derived combinators to work safely.
    ///
    pub def many1With(combine: (b, a) -> b, acc: b, ma: m[a]): m[b]

}



namespace Many {

    use Functor.{<$>};
    use Applicative.{<*>, <*, *>};

    ///
    /// Zero or more
    ///
    pub def many(ma: m[a]): m[List[a]] with Many[m] =
       let snoc = (k, a) -> ks -> k(a :: ks);
       Many.manyWith(snoc, ks -> ks, ma) |> Functor.map(k -> k(Nil))

    ///
    /// One or more
    ///
    pub def many1(ma: m[a]): m[List[a]] with Many[m] =
        let snoc = (k, a) -> ks -> k(a :: ks);
        Many.many1With(snoc, ks -> ks, ma) |> Functor.map(k -> k(Nil))

    ///
    /// Returns the count of many times the action `ma` succeeds
    ///
    pub def count(ma: m[a]): m[Int32] with Many[m]  =
        manyWith((ac, _) -> ac + 1, 0, ma)

    pub def skipMany(ma: m[a]): m[Unit] with Many[m]  =
        manyWith((ac, _) -> ac, (), ma)

    pub def skipMany1(ma: m[a]): m[Unit] with Many[m]  =
        many1With((ac, _) -> ac, (), ma)

    /// many seperated and ended by sep
    pub def endBy(ma: m[a], sep: m[sep]): m[List[a]] with Many[m] =
        many(ma <* sep)

    /// many1 seperated and ended by sep
    pub def endBy1(ma: m[a], sep: m[sep]): m[List[a]] with Many[m] =
        many1(ma <* sep)

    pub def manyTill(ma: m[a], stop: m[end]): m[List[a]] with Many[m] =
        many(ma) <* stop

    pub def manyTill1(ma: m[a], stop: m[end]): m[List[a]] with Many[m] =
        many1(ma) <* stop

    pub def skipManyTill(ma: m[a], stop: m[end]): m[Unit] with Many[m]  =
        skipMany(ma) <* stop

    pub def skipManyTill1(ma: m[a], stop: m[end]): m[Unit] with Many[m]  =
        skipMany1(ma) <* stop


    pub def sepBy(ma: m[a], sep: m[end]): m[List[a]] with Many[m] =
        Alternative.alt(sepBy1(ma, sep), Applicative.point(Nil))

    pub def sepBy1(ma: m[a], sep: m[end]): m[List[a]] with Many[m] =
        (((h, tl) -> h :: tl) <$> ma) <*> many(sep *> ma)

    pub def sepEndBy(ma: m[a], sep: m[end]): m[List[a]] with Many[m] =
        Alternative.alt(sepEndBy1(ma, sep), Applicative.point(Nil))

    pub def sepEndBy1(ma: m[a], sep: m[end]): m[List[a]] with Many[m] =
        ((((h, tl, _) -> h :: tl) <$> ma) <*> many(sep *> ma)) <*> Alternative.optional(sep)


    /// The prefix `m1` must succeed, `m2` runs zero-or-more times.
    pub def prefixMany(m1: m[a], m2: m[a]): m[List[a]] with Many[m] =
        (((h, tl) -> h :: tl) <$> m1) <*> many(m2)

    /// The prefix `m1` must succeed, `m2` runs one-or-more times.
    pub def prefixMany1(m1: m[a], m2: m[a]): m[List[a]] with Many[m] =
        (((h, tl) -> h :: tl) <$> m1) <*> many1(m2)

}
