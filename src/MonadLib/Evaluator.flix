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

namespace MonadLib {

    ///
    /// State is expected to be unchanged for the extent of an evaluation, 
    /// but it may be mutated outside the Evaluator monad. A `ResultSet` in 
    /// the JDBC library is a typical example. This has operations on the current
    /// row (`getString`, `getTime`, etc.) but also cursor level operations 
    /// to change to another row. An Evaluator for a ResultSet would only provide 
    /// the row operations and be expexcted to run for every row in a traversal
    /// of the results.
    ///
    /// The Evaluation monad is in the codensity style to be stack safe.
    /// 
    /// `ka` is the continuation answer type
    ///
    pub enum Evaluator[r: Region, ka: Type, st: Type, a: Type](
        st -> (a -> Result[String, ka] \ r) -> Result[String, ka] \ r)


    instance Functor[Evaluator[r, ka, st]] { 
        pub def map(f: a -> b \ ef, x: Evaluator[r, ka, st, a]): Evaluator[r, ka, st, b] \ ef = 
            MonadLib/Evaluator.map(f, x)
    }

    instance Applicative[Evaluator[r, ka, st]] { 
        pub def point(x: a): Evaluator[r, ka, st, a] = MonadLib/Evaluator.point(x)

        pub def ap(f: Evaluator[r, ka, st, a -> b \ ef], x: Evaluator[r, ka, st, a]): Evaluator[r, ka, st, b] \ ef = 
            MonadLib/Evaluator.ap(f, x)
    }

    instance Monad[Evaluator[r, ka, st]] { 
        pub def flatMap(k: a -> Evaluator[r, ka, st, b] \ ef, x: Evaluator[r, ka, st, a]): Evaluator[r, ka, st, b] \ ef = 
            MonadLib/Evaluator.flatMap(k, x)
    }

    instance MonadZero[Evaluator[r, ka, st]] { 
        pub def empty(): Evaluator[r, ka, st, a] = MonadLib/Evaluator.empty()
    }

    // Orelse available for Evaluator because it is expected to only be reading the state

    instance MonadLib.MonadOr[Evaluator[r, ka, st]] { 
        pub def orelse(ma: Evaluator[r, ka, st, a], mb: Evaluator[r, ka, st, a]): Evaluator[r, ka, st, a] =
            MonadLib/Evaluator.orelse(ma, mb)
    }

    instance MonadLib.MonadError[Evaluator[r, ka, st]] { 
        pub def throwError(msg: String): Evaluator[r, ka, st, a] =
            MonadLib/Evaluator.throwError(msg)

        pub def mapError(f: String -> String \ ef, ma: Evaluator[r, ka, st, a]): Evaluator[r, ka, st, a] \ ef = 
            MonadLib/Evaluator.mapError(f, ma)

        pub def catchError(ma: Evaluator[r, ka, st, a], hdlr: String -> Evaluator[r, ka, st, a] \ ef): Evaluator[r, ka, st, a] \ ef = 
            MonadLib/Evaluator.catchError(ma, hdlr)
    }  

}

namespace MonadLib/Evaluator {

    use MonadLib.Evaluator;
    use MonadLib.Evaluator.{Evaluator};




    def apply1(ma: Evaluator[r, ka, st, a], 
                s: st, 
                k: a -> Result[String, ka] \ r): Result[String, ka] \ r =
        let Evaluator(ma1) = ma;
        ma1(s, k)


    def apply1WithHandler(ma: Evaluator[r, ka, st, a], 
                            s: st, 
                            k: a -> Result[String, ka] \ r, 
                            handleK: String -> Result[String, ka] \ r): Result[String, ka] \ r =
        let Evaluator(ma1) = ma;
        match ma1(s, k) {
            case Err(msg) => handleK(msg)
            case Ok(a)    => Ok(a)
        }


    ///
    /// Returns the result of applying `ma` to the state `s`.
    ///
    pub def runEvaluator(_: Region[r], 
                        ma: Evaluator[r, a, st, a], 
                        s: st): Result[String, a] \ r =
        let Evaluator(f) = ma;
        let cont = a -> unsafe_cast Ok(a) as _ \ r;
        f(s, cont)

    pub def point(x: a): Evaluator[r, ka, st, a] = Evaluator((_, k) -> k(x))

    pub def map(f: a -> b \ ef, ma: Evaluator[r, ka, st, a]): Evaluator[r, ka, st, b] \ ef =
        unsafe_cast (Evaluator((s, k) ->
            apply1(ma, s, a -> k(unsafe_cast f(a) as _ \ r))
        )) as _ \ ef

    pub def ap(mf: Evaluator[r, ka, st, a -> b \ ef], ma: Evaluator[r, ka, st, a]): Evaluator[r, ka, st, b] \ ef =
        unsafe_cast (Evaluator((s, k) ->
            apply1(mf, s, f ->
                apply1(ma, s, a ->
                    k(unsafe_cast f(a) as _ \ r)))
        )) as _ \ ef


    pub def flatMap(f: a -> Evaluator[r, ka, st, b] \ ef, ma: Evaluator[r, ka, st, a]): Evaluator[r, ka, st, b] \ ef =
        unsafe_cast (Evaluator((s, k) ->
            apply1(ma, s, a ->
                apply1(unsafe_cast f(a) as _ \ r, s, b ->
                    k(b)))
        )) as _ \ ef

    pub def orelse(ma: Evaluator[r, ka, st, a], mb: Evaluator[r, ka, st, a]): Evaluator[r, ka, st, a] =
        Evaluator((s, k) ->
            apply1WithHandler(ma, s, a -> k(a), _ -> apply1(mb, s, b -> k(b)))
        )

    pub def throwError(msg: String): Evaluator[r, ka, st, a] = 
        Evaluator((_, _) -> unsafe_cast Err(msg) as _ \ r)

    pub def catchError(ma: Evaluator[r, ka, st, a], 
                        hdlr: String -> Evaluator[r, ka, st, a] \ ef): Evaluator[r, ka, st, a] \ ef= 
        unsafe_cast (Evaluator((s, k) -> 
            let Evaluator(ma1) = ma;
                unsafe_cast (match ma1(s, k) {
                    case Ok(a)    => Ok(a)
                    case Err(msg) => let Evaluator(hdlr1) = hdlr(msg); hdlr1(s, k)
                }) as _ \ r
        )) as _ \ ef



    pub def mapError(f: String -> String \ ef, ma: Evaluator[r, ka, st, a]): Evaluator[r, ka, st, a] \ ef = 
        unsafe_cast (Evaluator((s, k) -> 
            apply1WithHandler(ma, s, a -> k(a), msg -> unsafe_cast Err(f(msg)) as _ \ r)
        )) as _ \ ef

    pub def empty(): Evaluator[r, ka, st, a] = throwError("empty")


    pub def liftGetter(f: st -> a \ ef): Evaluator[r, ka, st, a] \ ef = 
        unsafe_cast (Evaluator((s, k) ->
            try {
                let ans = unsafe_cast f(s) as _ \ r;
                k(ans)
            } catch {
                case ex: ##java.lang.Exception => 
                    import java.lang.Throwable.getMessage(): String \ r;
                    Err(getMessage(ex))
            }            
        )) as _ \ ef

    // f should not throw an exception
    pub def liftGetterResult(f: st -> Result[e, a] \ ef): Evaluator[r, ka, st, a] \ ef with ToString[e] = 
        unsafe_cast (Evaluator((s, k) ->
            match (unsafe_cast f(s) as _ \ r) { 
                case Ok(a)  => k(a)
                case Err(e) => Err(ToString.toString(e))
            } 
        )) as _ \ ef


    pub def liftAction(f: Unit -> a \ ef): Evaluator[r, ka, st, a] \ ef = 
        unsafe_cast (Evaluator((_, k) ->
            let a = unsafe_cast f() as _ \ r;
            k(a)
        )) as _ \ ef

    pub def liftActionResult(f: Unit -> Result[e, a] \ ef): Evaluator[r, ka, st, a] \ ef with ToString[e] = 
        unsafe_cast (Evaluator((_, k) ->
            match (unsafe_cast f() as _ \ r) {
                case Ok(a)  => k(a)
                case Err(e) => Err(ToString.toString(e))
            }
        )) as _ \ ef

}

