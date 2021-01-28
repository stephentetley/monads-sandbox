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

namespace SmallParser {


    type alias ParseResult[a] = Result[(a, List[String]), String]

    pub opaque type Parser[a] = List[String] ~> ParseResult[a]

    
    ///
    /// Run parser `p` on the token list `input`.
    ///
    pub def run(p: Parser[t], input: List[String]) : Result[t, String] & Impure =
        let Parser(p1) = p;
        p1(input) |> Result.map(fst)


    pub def map(f: a -> b & ef, p: Parser[a]): Parser[b] =
        let Parser(p1) = p;
        Parser(inp ->
            match p1(inp) {
                case Ok((a, toks)) => Ok((f(a) as & Impure, toks))
                case Err(msg) => Err(msg)
            }
        )

    /// instance Functor[Parser] {
    ///     def map(f: a -> b & ef, x: Parser[a]): Parser[b] = map(f, x)
    /// }


}
