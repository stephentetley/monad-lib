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
    /// Evaluator monad with an "environment" aka Reader.
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
    pub enum EnvEvaluator[r: Region, ka: Type, env: Type, st: Type, a: Type](
        env -> st -> (a -> Result[String, ka] \ r) -> Result[String, ka] \ r)


    instance Functor[EnvEvaluator[r, ka, env, st]] { 
        pub def map(f: a -> b \ ef, x: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, b] \ ef = 
            MonadLib/EnvEvaluator.map(f, x)
    }

    instance Applicative[EnvEvaluator[r, ka, env, st]] { 
        pub def point(x: a): EnvEvaluator[r, ka, env, st, a] = 
            MonadLib/EnvEvaluator.point(x)

        pub def ap(f: EnvEvaluator[r, ka, env, st, a -> b \ ef], x: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, b] \ ef = 
            MonadLib/EnvEvaluator.ap(f, x)
    }

    instance Monad[EnvEvaluator[r, ka, env, st]] { 
        pub def flatMap(k: a -> EnvEvaluator[r, ka, env, st, b] \ ef, x: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, b] \ ef = 
            MonadLib/EnvEvaluator.flatMap(k, x)
    }

    instance MonadZero[EnvEvaluator[r, ka, env, st]] { 
        pub def empty(): EnvEvaluator[r, ka, env, st, a] = 
            MonadLib/EnvEvaluator.empty()
    }

    // Orelse available for Evaluator because it is expected to only be reading the state

    instance MonadLib.MonadOr[EnvEvaluator[r, ka, env, st]] { 
        pub def orelse(ma: EnvEvaluator[r, ka, env, st, a], mb: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, a] =
            MonadLib/EnvEvaluator.orelse(ma, mb)
    }

    instance MonadLib.MonadError[EnvEvaluator[r, ka, env, st]] { 
        pub def throwError(msg: String): EnvEvaluator[r, ka, env, st, a] =
            MonadLib/EnvEvaluator.throwError(msg)

        pub def mapError(f: String -> String \ ef, ma: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, a] \ ef = 
            MonadLib/EnvEvaluator.mapError(f, ma)

        pub def catchError(ma: EnvEvaluator[r, ka, env, st, a], hdlr: String -> EnvEvaluator[r, ka, env, st, a] \ ef): EnvEvaluator[r, ka, env, st, a] \ ef = 
            MonadLib/EnvEvaluator.catchError(ma, hdlr)
    }  

}

namespace MonadLib/EnvEvaluator {

    use MonadLib.EnvEvaluator;
    use MonadLib.EnvEvaluator.{EnvEvaluator};

    def apply1(ma: EnvEvaluator[r, ka, env, st, a], 
                e: env,
                s: st, 
                k: a -> Result[String, ka] \ r): Result[String, ka] \ r =
        let EnvEvaluator(ma1) = ma;
        ma1(e, s, k)


    def apply1WithHandler(ma: EnvEvaluator[r, ka, env, st, a], 
                            e: env,
                            s: st, 
                            k: a -> Result[String, ka] \ r, 
                            handleK: String -> Result[String, ka] \ r): Result[String, ka] \ r =
        let EnvEvaluator(ma1) = ma;
        match ma1(e, s, k) {
            case Err(msg) => handleK(msg)
            case Ok(a)    => Ok(a)
        }


    ///
    /// Returns the result of applying `ma` to the state `s`.
    ///
    pub def runEnvEvaluator(_: Region[r], 
                            ma: EnvEvaluator[r, a, env, st, a], 
                            e: env,
                            s: st): Result[String, a] \ r =
        let EnvEvaluator(f) = ma;
        let cont = a -> unsafe_cast Ok(a) as _ \ r;
        f(e, s, cont)

    pub def point(x: a): EnvEvaluator[r, ka, env, st, a] = 
        EnvEvaluator((_, _, k) -> k(x))

    pub def map(f: a -> b \ ef, ma: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, b] \ ef =
        unsafe_cast (EnvEvaluator((e, s, k) ->
            apply1(ma, e, s, a -> k(unsafe_cast f(a) as _ \ r))
        )) as _ \ ef

    pub def ap(mf: EnvEvaluator[r, ka, env, st, a -> b \ ef], ma: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, b] \ ef =
        unsafe_cast (EnvEvaluator((e, s, k) ->
            apply1(mf, e, s, f ->
                apply1(ma, e, s, a ->
                    k(unsafe_cast f(a) as _ \ r)))
        )) as _ \ ef


    pub def flatMap(f: a -> EnvEvaluator[r, ka, env, st, b] \ ef, ma: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, b] \ ef =
        unsafe_cast (EnvEvaluator((e, s, k) ->
            apply1(ma, e, s, a ->
                apply1(unsafe_cast f(a) as _ \ r, e, s, b ->
                    k(b)))
        )) as _ \ ef

    pub def throwError(msg: String): EnvEvaluator[r, ka, env, st, a] = 
        EnvEvaluator((_, _, _) -> unsafe_cast Err(msg) as _ \ r)

    pub def catchError(ma: EnvEvaluator[r, ka, env, st, a], 
                        hdlr: String -> EnvEvaluator[r, ka, env, st, a] \ ef): EnvEvaluator[r, ka, env, st, a] \ ef= 
        unsafe_cast (EnvEvaluator((e, s, k) -> 
            let EnvEvaluator(ma1) = ma;
                unsafe_cast (match ma1(e, s, k) {
                    case Ok(a)    => Ok(a)
                    case Err(msg) => let EnvEvaluator(hdlr1) = hdlr(msg); hdlr1(e, s, k)
                }) as _ \ r
        )) as _ \ ef



    pub def mapError(f: String -> String \ ef, ma: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, a] \ ef = 
        unsafe_cast (EnvEvaluator((e, s, k) -> 
            apply1WithHandler(ma, e, s, a -> k(a), msg -> unsafe_cast Err(f(msg)) as _ \ r)
        )) as _ \ ef

    pub def empty(): EnvEvaluator[r, ka, env, st, a] = throwError("empty")

    pub def orelse(ma: EnvEvaluator[r, ka, env, st, a], mb: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, a] =
        EnvEvaluator((e, s, k) ->
            apply1WithHandler(ma, e, s, a -> k(a), _ -> apply1(mb, e, s, b -> k(b)))
        )

    pub def liftGetter(f: env -> st -> a \ ef): EnvEvaluator[r, ka, env, st, a] \ ef = 
        unsafe_cast (EnvEvaluator((e, s, k) ->
            try {
                let ans = unsafe_cast f(e, s) as _ \ r;
                k(ans)
            } catch {
                case ex: ##java.lang.Exception => 
                    import java.lang.Throwable.getMessage(): String \ r;
                    Err(getMessage(ex))
            }            
        )) as _ \ ef

    // f should not throw an exception
    pub def liftGetterResult(f: env -> st -> Result[e, a] \ ef): EnvEvaluator[r, ka, env, st, a] \ ef with ToString[e] = 
        unsafe_cast (EnvEvaluator((e, s, k) ->
            match (unsafe_cast f(e, s) as _ \ r) { 
                case Ok(a)    => k(a)
                case Err(err) => Err(ToString.toString(err))
            } 
        )) as _ \ ef


    pub def liftAction(f: Unit -> a \ ef): EnvEvaluator[r, ka, env, st, a] \ ef = 
        unsafe_cast (EnvEvaluator((_, _, k) ->
            let a = unsafe_cast f() as _ \ r;
            k(a)
        )) as _ \ ef

    pub def liftActionResult(f: Unit -> Result[e, a] \ ef): EnvEvaluator[r, ka, env, st, a] \ ef with ToString[e] = 
        unsafe_cast (EnvEvaluator((_, _, k) ->
            match (unsafe_cast f() as _ \ r) {
                case Ok(a)    => k(a)
                case Err(err) => Err(ToString.toString(err))
            }
        )) as _ \ ef

    pub def askEnv(): EnvEvaluator[r, ka, env, st, env]  = 
        EnvEvaluator((e, _, k) -> k(e))

    pub def asksEnv(f: env -> a): EnvEvaluator[r, ka, env, st, a]  = 
        EnvEvaluator((e, _, k) -> k(f(e)))

    pub def localEnv(f: env -> env1 \ ef, ma: EnvEvaluator[r, ka, env1, st, a]): EnvEvaluator[r, ka, env, st, a] \ ef = 
        unsafe_cast (EnvEvaluator((e, s, k) -> 
            apply1(ma, unsafe_cast f(e) as _ \ r, s, a1 -> 
                k(a1))
        )) as _ \ ef

}

