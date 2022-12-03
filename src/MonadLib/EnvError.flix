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
    /// Reader and Error monad in codensity style to be stack safe.
    /// 
    /// `ka` is the continuation answer type
    ///
    pub enum EnvError[r: Region, ka: Type, env: Type, a: Type](
        env -> (a -> Result[ka, String] \ r) -> Result[ka, String] \ r)


    instance Functor[EnvError[r, ka, env]] {
        pub def map(f: a -> b \ ef, ma: EnvError[r, ka, env, a]): EnvError[r, ka, env, b] \ ef = 
            MonadLib/EnvError.map(f, ma)
    }


    instance Applicative[EnvError[r, ka, env]] {
        pub def point(x: a): EnvError[r, ka, env, a] =  
            MonadLib/EnvError.point(x)

        pub def ap(mf: EnvError[r, ka, env, a -> b \ ef], ma: EnvError[r, ka, env, a]): EnvError[r, ka, env, b] \ ef =
            MonadLib/EnvError.ap(mf, ma)
    }

    instance Monad[EnvError[r, ka, env]] {
        pub def flatMap(f: a -> EnvError[r, ka, env, b] \ ef, ma: EnvError[r, ka, env, a]): EnvError[r, ka, env, b] \ ef =
            MonadLib/EnvError.flatMap(f, ma)
    }

    instance MonadZero[EnvError[r, ka, env]] { 
        pub def empty(): EnvError[r, ka, env, a] = 
            MonadLib/EnvError.empty()
    }

    // Orelse available for Evaluator because it is expected to only be reading the state

    instance MonadLib.MonadOr[EnvError[r, ka, env]] { 
        pub def orelse(ma: EnvError[r, ka, env, a], mb: EnvError[r, ka, env, a]): EnvError[r, ka, env, a] =
            MonadLib/EnvError.orelse(ma, mb)
    }
    
    instance MonadLib.MonadError[EnvError[r, ka, env]] { 
        pub def throwError(msg: String): EnvError[r, ka, env, a] =
            MonadLib/EnvError.throwError(msg)

        pub def mapError(f: String -> String \ ef, ma: EnvError[r, ka, env, a]): EnvError[r, ka, env, a] \ ef = 
            MonadLib/EnvError.mapError(f, ma)

        pub def catchError(ma: EnvError[r, ka, env, a], hdlr: String -> EnvError[r, ka, env, a] \ ef): EnvError[r, ka, env, a] \ ef = 
            MonadLib/EnvError.catchError(ma, hdlr)
    }  

}


namespace MonadLib/EnvError {

    use MonadLib.EnvError;
    use MonadLib.EnvError.{EnvError};


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: EnvError[r, ka, env, a],
                e: env,
                k: a -> Result[ka, String] \ r): Result[ka, String] \ r =
        let EnvError(ma1) = ma;
        ma1(e, k)

    def apply1WithHandler(ma: EnvError[r, ka, env, a], 
                            e: env,
                            k: a -> Result[ka, String] \ r, 
                            handleK: String -> Result[ka, String] \ r): Result[ka, String] \ r =
        let EnvError(ma1) = ma;
        match ma1(e, k) {
            case Err(msg) => handleK(msg)
            case Ok(a)    => Ok(a)
        }

    ///
    /// Returns the result of applying `ma` to the state `s`.
    ///
    pub def runEnvError(_: Region[r], ma: EnvError[r, a, env, a], e: env): Result[a, String] \ r =
        let EnvError(f) = ma;
        let cont = a -> unsafe_cast Ok(a) as \ r;
        f(e, cont)


    pub def point(x: a): EnvError[r, ka, env, a] = 
        EnvError((_, k) -> k(x))

    pub def map(f: a -> b \ ef, ma: EnvError[r, ka, env, a]): EnvError[r, ka, env, b] \ ef =
        unsafe_cast (EnvError((e, k) ->
            apply1(ma, e, a -> k(unsafe_cast f(a) as \ r))
        )) as \ ef

    pub def ap(mf: EnvError[r, ka, env, a -> b \ ef], ma: EnvError[r, ka, env, a]): EnvError[r, ka, env, b] \ ef =
        unsafe_cast (EnvError((e, k) ->
            apply1(mf, e, f ->
                apply1(ma, e, a ->
                    k(unsafe_cast f(a) as \ r)))
        )) as \ ef


    pub def flatMap(f: a -> EnvError[r, ka, env, b] \ ef, ma: EnvError[r, ka, env, a]): EnvError[r, ka, env, b] \ ef =
        unsafe_cast (EnvError((e, k) ->
            apply1(ma, e, a ->
                apply1(unsafe_cast f(a) as \ r, e, b ->
                    k(b)))
        )) as \ ef

    pub def throwError(msg: String): EnvError[r, ka, env, a] = 
        EnvError((_, _) -> unsafe_cast Err(msg) as \ r)

    pub def catchError(ma: EnvError[r, ka, env, a], 
                        hdlr: String -> EnvError[r, ka, env, a] \ ef): EnvError[r, ka, env, a] \ ef= 
        unsafe_cast (EnvError((e, k) -> 
            let EnvError(ma1) = ma;
                unsafe_cast (match ma1(e, k) {
                    case Ok(a)    => Ok(a)
                    case Err(msg) => let EnvError(hdlr1) = hdlr(msg); hdlr1(e, k)
                }) as \ r
        )) as \ ef

    pub def mapError(f: String -> String \ ef, ma: EnvError[r, ka, env, a]): EnvError[r, ka, env, a] \ ef = 
        unsafe_cast (EnvError((e, k) -> 
            apply1WithHandler(ma, e, a -> k(a), msg -> unsafe_cast Err(f(msg)) as \ r)
        )) as \ ef

    pub def empty(): EnvError[r, ka, env, a] = throwError("empty")

    pub def orelse(ma: EnvError[r, ka, env, a], mb: EnvError[r, ka, env, a]): EnvError[r, ka, env, a] =
        EnvError((e, k) ->
            apply1WithHandler(ma, e, a -> k(a), _ -> apply1(mb, e, b -> k(b)))
        )


    pub def liftGetter(f: env -> a \ ef): EnvError[r, ka, env, a] \ ef = 
        unsafe_cast (EnvError((e, k) ->
            try {
                let ans = unsafe_cast f(e) as \ r;
                k(ans)
            } catch {
                case ex: ##java.lang.Exception => 
                    import java.lang.Throwable.getMessage(): String \ r;
                    Err(getMessage(ex))
            }
        )) as \ ef

    // f should not throw an exception
    pub def liftGetterResult(f: env -> Result[a, err] \ ef): EnvError[r, ka, env, a] \ ef with ToString[err] = 
        unsafe_cast (EnvError((e, k) ->
            match (unsafe_cast f(e) as \ r) { 
                case Ok(a)    => k(a)
                case Err(err) => Err(ToString.toString(err))
            } 
        )) as \ ef


    pub def liftAction(f: Unit -> a \ ef): EnvError[r, ka, env, a] \ ef = 
        unsafe_cast (EnvError((_, k) ->
            let a = unsafe_cast f() as \ r; 
            k(a)  
        )) as \ ef

    pub def liftActionResult(f: Unit -> Result[a, err] \ ef): EnvError[r, ka, env, a] \ ef with ToString[err] = 
        unsafe_cast (EnvError((_, k) ->
            match (unsafe_cast f() as \ r) {
                case Ok(a)    => k(a)
                case Err(err) => Err(ToString.toString(err))
            } 
        )) as \ ef


    pub def askEnv(): EnvError[r, ka, env, env] = 
        EnvError((e, k) -> k(e))

    pub def asksEnv(f: env -> a): EnvError[r, ka, env, a] = 
        EnvError((e, k) -> k(f(e)))

    pub def localEnv(f: env -> env1 \ ef, ma: EnvError[r, ka, env1, a]): EnvError[r, ka, env, a] \ ef = 
        unsafe_cast (EnvError((e, k) -> 
            apply1(ma, unsafe_cast f(e) as \ r, a1 -> 
                k(a1))
        )) as \ ef

}
