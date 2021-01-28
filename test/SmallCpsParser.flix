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

namespace SmallCpsParser {


    type alias Failures = Chain[String]


    type alias FailureK[t] = Failures ~> t

    type alias SuccessK[t, a] = a -> FailureK[t] -> List[String] -> Failures ~> t

    pub opaque type Parser[t, a] = SuccessK[t, a] -> FailureK[t] -> List[String] -> Failures ~> t

    type alias Parser1[a] = Parser[Result[a, List[String]], a]

    ///
    /// Run parser `p` on the token list `input`.
    ///
    pub def run(p: Parser1[t], input: List[String]) : Result[t, String] & Impure =
        use Chain.{toList, empty};
        let fk = errs -> Err(toList(errs)) as & Impure;
        let sk = (a, _, _, _) -> Ok(a) as & Impure;
        let Parser(p1) = p;
        match p1(sk, fk, input, empty()) { 
            case Ok(a) => Ok(a)
            case Err(xs) => Err(String.unlines(xs))
        }

    pub def apply1 (p: Parser[t, a], sk: SuccessK[t, a], fk: FailureK[t], inp: List[String], errs: Failures): t & Impure =
        let Parser(pf) = p;
        pf(sk, fk, inp, errs)

    pub def map(f: a -> b & ef, p: Parser[t, a]): Parser[t, b] =
        Parser((sk, fk, inp, errs) ->
            let sk1 = (x, fk1, inp1, errs1) -> sk(f(x) as & Impure, fk1, inp1, errs1);
            apply1(p, sk1, fk, inp, errs)
        )

    /// instance Functor[Parser[t]] {
    ///     def map(f: a -> b & ef, x: Parser[t, a]): Parser[t, b] & ef = map(f, x)
    /// }


}
