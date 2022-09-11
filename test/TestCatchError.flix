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

namespace TestCatchError {

    use Applicative.{*>};
    use MonadLib/MonadError.{throwError, catchError};

    pub type alias SM[r: Region, ka: Type, a: Type] = MonadLib.MutStateError[r, ka, Ref[Int32, r], a]

    pub def incr1!(): SM[r, ka, Unit] \ Write(r) = 
        MonadLib/MutStateError.liftSetter!(s -> let s1 = deref s; s := s1 + 1)

    pub def getSt(): SM[r, ka, Int32] \ Write(r) = 
        MonadLib/MutStateError.liftGetter(s -> deref s)

    @test
    def test01(): Bool = 
        let ans = region r {
            let st = ref 0 @ r;
            MonadLib/MutStateError.runMutStateError(r, incr1!() *> getSt(), st)
        };
        ans == Ok(1)

    @test
    def test02(): Bool = 
        let ans = region r {
            let st = ref 0 @ r;
            MonadLib/MutStateError.runMutStateError(r, catchError(throwError("err1"), _ ->  incr1!() *> incr1!() *> getSt()), st)
        };
        ans == Ok(2)


    @test
    def test03(): Bool = 
        let ans = region r {
            let st = ref 0 @ r;
            MonadLib/MutStateError.runMutStateError(r, catchError(incr1!() *> getSt(), _ ->  incr1!() *> incr1!() *> getSt()), st)
        };
        ans == Ok(1)

}


