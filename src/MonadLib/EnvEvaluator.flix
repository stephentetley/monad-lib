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
        env -> st -> (a -> Result[ka, String] \ r) -> Result[ka, String] \ r)


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
                k: a -> Result[ka, String] \ r): Result[ka, String] \ r =
        let EnvEvaluator(ma1) = ma;
        ma1(e, s, k)


    def apply1WithHandler(ma: EnvEvaluator[r, ka, env, st, a], 
                            e: env,
                            s: st, 
                            k: a -> Result[ka, String] \ r, 
                            handleK: String -> Result[ka, String] \ r): Result[ka, String] \ r =
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
                            s: st): Result[a, String] \ r =
        let EnvEvaluator(f) = ma;
        let cont = a -> Ok(a) as \ r;
        f(e, s, cont)

    pub def point(x: a): EnvEvaluator[r, ka, env, st, a] = 
        EnvEvaluator((_, _, k) -> k(x))

    pub def map(f: a -> b \ ef, ma: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, b] \ ef =
        EnvEvaluator((e, s, k) ->
            apply1(ma, e, s, a -> k(f(a) as \ r))
        ) as \ ef

    pub def ap(mf: EnvEvaluator[r, ka, env, st, a -> b \ ef], ma: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, b] \ ef =
        EnvEvaluator((e, s, k) ->
            apply1(mf, e, s, f ->
                apply1(ma, e, s, a ->
                    k(f(a) as \ r)))
        ) as \ ef


    pub def flatMap(f: a -> EnvEvaluator[r, ka, env, st, b] \ ef, ma: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, b] \ ef =
        EnvEvaluator((e, s, k) ->
            apply1(ma, e, s, a ->
                apply1(f(a) as \ r, e, s, b ->
                    k(b)))
        ) as \ ef

    pub def orelse(ma: EnvEvaluator[r, ka, env, st, a], mb: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, a] =
        EnvEvaluator((e, s, k) ->
            apply1WithHandler(ma, e, s, a -> k(a), _ -> apply1(mb, e, s, b -> k(b)))
        )

    pub def throwError(msg: String): EnvEvaluator[r, ka, env, st, a] = 
        EnvEvaluator((_, _, _) -> Err(msg) as \ r)

    pub def catchError(ma: EnvEvaluator[r, ka, env, st, a], 
                        hdlr: String -> EnvEvaluator[r, ka, env, st, a] \ ef): EnvEvaluator[r, ka, env, st, a] \ ef= 
        EnvEvaluator((e, s, k) -> 
            let EnvEvaluator(ma1) = ma;
                match ma1(e, s, k) {
                    case Ok(a)    => Ok(a)
                    case Err(msg) => let EnvEvaluator(hdlr1) = hdlr(msg); hdlr1(e, s, k)
                } as \ r
        ) as \ ef



    pub def mapError(f: String -> String \ ef, ma: EnvEvaluator[r, ka, env, st, a]): EnvEvaluator[r, ka, env, st, a] \ ef = 
        EnvEvaluator((e, s, k) -> 
            apply1WithHandler(ma, e, s, a -> k(a), msg -> Err(f(msg)) as \ r)
        ) as \ ef

    pub def empty(): EnvEvaluator[r, ka, env, st, a] = throwError("empty")


    pub def liftGetter(f: env -> st -> a \ ef): EnvEvaluator[r, ka, env, st, a] \ ef = 
        EnvEvaluator((e, s, k) ->
            try {
                let ans = f(e, s) as \ r;
                k(ans)
            } catch {
                case ex: ##java.lang.Exception => 
                    import java.lang.Throwable.getMessage(): String \ r;
                    Err(getMessage(ex))
            }            
        ) as \ ef

    // f should not throw an exception
    pub def liftGetterResult(f: env -> st -> Result[a, err] \ ef): EnvEvaluator[r, ka, env, st, a] \ ef with ToString[err] = 
        EnvEvaluator((e, s, k) ->
            match f(e, s) { 
                case Ok(a)    => k(a)
                case Err(err) => Err(ToString.toString(err))
            } as \ r
        ) as \ ef


    pub def liftAction(f: Unit -> a \ ef): EnvEvaluator[r, ka, env, st, a] \ ef = 
        EnvEvaluator((_, _, k) ->
            let a =  f() as \ r;
            k(a)
        ) as \ ef

    pub def liftActionResult(f: Unit -> Result[a, err] \ ef): EnvEvaluator[r, ka, env, st, a] \ ef with ToString[err] = 
        EnvEvaluator((_, _, k) ->
            match {f() as \ r} {
                case Ok(a)    => k(a)
                case Err(err) => Err(ToString.toString(err))
            }
        ) as \ ef

}

