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



namespace MonadLib/Monad/Reader {

    ///
    /// Pure Reader monad implemented in CPS. 
    ///
    pub opaque type Reader[ka, r, a] = r -> (a -> ka) -> ka

    ///
    /// Helper function for Reader's implementations of `map` and `ap`.
    ///
    def apply1(ma: Reader[ka, r, a], r: r, k: a -> ka): ka =
        let Reader(f) = ma;
        f(r, k)


    ///
    /// Returns the result of applying `ma` to `r`.
    ///
    pub def runReader(ma: Reader[a, r, a], r: r): a =
        let Reader(f) = ma;
        f(r, k -> k)


    instance Functor[Reader[ka, r]] {
        pub def map(f: a -> b & ef, ma: Reader[ka, r, a]): Reader[ka, r, b] & ef =
            Reader((r, k) ->
                apply1(ma, r, v1 -> k(f(v1) as & Pure))
            ) as & ef
    }

    instance Applicative[Reader[ka, r]] {
        pub def point(x: a): Reader[ka, r, a] =
            Reader((_, k) -> k(x))

        pub def ap(mf: Reader[ka, r, a -> b & ef], ma: Reader[ka, r, a]): Reader[ka, r, b] & ef =
            Reader((r, k) ->
                apply1(mf, r, f1 ->
                    apply1(ma, r, a1 ->
                        k(f1(a1) as & Pure)))
            ) as & ef
    }

    instance Monad[Reader[ka, r]] {
        pub def flatMap(f: a -> Reader[ka, r, b] & ef, ma: Reader[ka, r, a]): Reader[ka, r, b] & ef =
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> Reader[ka, r, b] & ef, ma: Reader[ka, r, a]): Reader[ka, r, b] & ef =
        Reader((r, k) ->
            apply1(ma, r, a1 ->
                apply1(f(a1) as & Pure, r, b1 ->
                    k(b1)))
        ) as & ef
        
    pub def ask(): Reader[ka, r, r] = 
        Reader((r, k) -> 
            k(r)
        )

    pub def asks(f: s -> a & ef): Reader[ka, s, a] & ef = 
        Reader((r, k) -> 
            k(f(r) as & Pure)
        ) as & ef

    pub def local(f: r -> r, ma: Reader[ka, r, a]): Reader[ka, r, a] & ef = 
        Reader((r, k) -> 
            apply1(ma, f(r), a1 -> 
                k(a1))
        ) as & ef

}
