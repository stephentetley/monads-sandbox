/*
 * Copyright 2022 Stephen Tetley
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




namespace MonadLib/Monad/Simple/StringWriter {

    ///
    /// MutState monad with encapsulated effect and region
    ///
    pub enum StringWriter[r: Region, a: Type](StringBuilder[r] -> a)


    instance Functor[StringWriter[r]] {
        pub def map(f: a -> b & ef, ma: StringWriter[r, a]): StringWriter[r, b] & ef =
            StringWriter(sb ->
                apply1(ma, sb) |> v1 -> f(v1) as & Pure
            ) as & ef
    }


    instance Applicative[StringWriter[r]] {
        pub def point(x: a): StringWriter[r, a] = 
            StringWriter(_ -> x)

        pub def ap(mf: StringWriter[r, a -> b & ef], ma: StringWriter[r, a]): StringWriter[r, b] & ef =
            StringWriter(sb ->
                apply1(mf, sb) |> f1 ->
                    apply1(ma, sb) |> a1 ->
                        f1(a1) as & Pure
            ) as & ef
    }

    instance Monad[StringWriter[r]] {
        pub def flatMap(f: a -> StringWriter[r, b] & ef, ma: StringWriter[r, a]): StringWriter[r, b] & ef =
            flatMap(f, ma)
    }



    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: StringWriter[r, a], sb: StringBuilder[r]): a =
        let StringWriter(f) = ma;
        f(sb)



    pub def flatMap(f: a -> StringWriter[r, b] & ef, ma: StringWriter[r, a]): StringWriter[r, b] & ef =
        StringWriter(sb ->
            apply1(ma, sb) |> v1 ->
                apply1(f(v1) as & Pure, sb)
        ) as & ef


    pub def tell(s: String): StringWriter[r, Unit] \ Write(r) =
        StringWriter(sb -> StringBuilder.append!(s, sb) as & Pure) as \ {Write(r)}



    pub def runStringWriter(r: Region[r], m: StringWriter[r, a]): (String, a) \ { Read(r), Write(r) } =
        let StringWriter(m1) = m;
        let sb = new StringWBuilder(r);
        let ans = m1(sb);
        let s = StringBuilder.toString(sb);
        (s, ans)


    pub def demo01(): (String, Unit) =
        region r {
            runStringWriter(r, tell("Hello World!"))
        }

}
