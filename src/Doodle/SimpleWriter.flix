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

namespace RelLib {
    ///
    /// Simple Writer (not CPS)
    ///
    pub opaque type SimpleWriter[w: SchemaRow, a: Type] = (#{ | w}, a)


    instance Functor[SimpleWriter[w]] {
        pub def map(f: a -> b & ef, ma: SimpleWriter[w, a]): SimpleWriter[w, b] & ef =
            RelLib/SimpleWriter.map(f, ma)
    }


    instance Applicative[SimpleWriter[w]] {
        pub def point(x: a): SimpleWriter[w, a] =
            RelLib/SimpleWriter.point(x)

        pub def ap(mf: SimpleWriter[w, a -> b & ef], ma: SimpleWriter[w, a]): SimpleWriter[w, b] & ef =
            RelLib/SimpleWriter.ap(mf, ma)
    }


    instance Monad[SimpleWriter[w]] {
        pub def flatMap(f: a -> SimpleWriter[w, b] & ef, ma: SimpleWriter[w, a]): SimpleWriter[w, b] & ef =
            RelLib/SimpleWriter.flatMap(f, ma)
    }


}


namespace RelLib/SimpleWriter {

    use RelLib.SimpleWriter;
    use RelLib.SimpleWriter.{SimpleWriter};


    ///
    /// Returns the result of `ma`.
    ///
    pub def runSimpleWriter(ma: SimpleWriter[w, a]): (#{| w}, a)  =
        let SimpleWriter((w, a)) = ma;
        (w, a)


    ///
    /// Returns just the "output" of `ma`.
    ///
    pub def execSimpleWriter(ma: SimpleWriter[w, a]): #{ | w} =
        let SimpleWriter((w, _)) = ma;
        w


    pub def map(f: a -> b & ef, ma: SimpleWriter[w, a]): SimpleWriter[w, b] & ef =
        let SimpleWriter((w, a)) = ma;
        SimpleWriter((w, f(a) as & Pure)) as & ef


    pub def point(x: a): SimpleWriter[w, a]  =
        SimpleWriter((#{}, x))


    pub def ap(mf: SimpleWriter[w, a -> b & ef], ma: SimpleWriter[w, a]): SimpleWriter[w, b] & ef =
        let SimpleWriter((w1, f)) = mf;
        let SimpleWriter((w2, a)) = ma;
        SimpleWriter((w1 <+> w2, f(a) as & Pure)) as & ef



    pub def flatMap(f: a -> SimpleWriter[w, b] & ef, ma: SimpleWriter[w, a]): SimpleWriter[w, b] & ef =
        let SimpleWriter((w1, a)) = ma;
        let SimpleWriter((w2, b)) = f(a) as & Pure;
        SimpleWriter((w1 <+> w2, b)) as & ef


    pub def tell(a: #{| w}): SimpleWriter[w, Unit] =
        SimpleWriter((a, ()))


    pub def whenTrue(test: Bool, ma: SimpleWriter[w, Unit]): SimpleWriter[w, Unit] = match test {
        case true => ma
        case _    => point()
    }

}