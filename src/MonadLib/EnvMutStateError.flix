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
    /// Mutable State (no local), Env/Reader (with local), and Error 
    /// monad in codensity style to be stack safe.
    /// 
    /// `ka` is the continuation answer type
    ///
    pub enum EnvMutStateError[r: Region, ka: Type, env: Type, st: Type, a: Type](
        env -> st -> (a -> Result[ka, String] \ r) -> Result[ka, String] \ r)


    instance Functor[EnvMutStateError[r, ka, env, st]] {
        pub def map(f: a -> b \ ef, ma: EnvMutStateError[r, ka, env, st, a]): EnvMutStateError[r, ka, env, st, b] \ ef = 
            MonadLib/EnvMutStateError.map(f, ma)
    }


    instance Applicative[EnvMutStateError[r, ka, env, st]] {
        pub def point(x: a): EnvMutStateError[r, ka, env, st, a] =  
            MonadLib/EnvMutStateError.point(x)

        pub def ap(mf: EnvMutStateError[r, ka, env, st, a -> b \ ef], ma: EnvMutStateError[r, ka, env, st, a]): EnvMutStateError[r, ka, env, st, b] \ ef =
            MonadLib/EnvMutStateError.ap(mf, ma)
    }

    instance Monad[EnvMutStateError[r, ka, env, st]] {
        pub def flatMap(f: a -> EnvMutStateError[r, ka, env, st, b] \ ef, ma: EnvMutStateError[r, ka, env, st, a]): EnvMutStateError[r, ka, env, st, b] \ ef =
            MonadLib/EnvMutStateError.flatMap(f, ma)
    }

    /// No `MonadOr` for EnvMutStateError because updated state cannot be unwound.
    
    instance MonadLib.MonadError[EnvMutStateError[r, ka, env, st]] { 
        pub def throwError(msg: String): EnvMutStateError[r, ka, env, st, a] =
            MonadLib/EnvMutStateError.throwError(msg)

        pub def mapError(f: String -> String \ ef, ma: EnvMutStateError[r, ka, env, st, a]): EnvMutStateError[r, ka, env, st, a] \ ef = 
            MonadLib/EnvMutStateError.mapError(f, ma)

        pub def catchError(ma: EnvMutStateError[r, ka, env, st, a], hdlr: String -> EnvMutStateError[r, ka, env, st, a] \ ef): EnvMutStateError[r, ka, env, st, a] \ ef = 
            MonadLib/EnvMutStateError.catchError(ma, hdlr)
    }  

}


namespace MonadLib/EnvMutStateError {

    use MonadLib.EnvMutStateError;
    use MonadLib.EnvMutStateError.{EnvMutStateError};


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: EnvMutStateError[r, ka, env, st, a],
                e: env,
                s: st, 
                k: a -> Result[ka, String] \ r): Result[ka, String] \ r =
        let EnvMutStateError(ma1) = ma;
        ma1(e, s, k)

    def apply1WithHandler(ma: EnvMutStateError[r, ka, env, st, a], 
                            e: env,
                            s: st, 
                            k: a -> Result[ka, String] \ r, 
                            handleK: String -> Result[ka, String] \ r): Result[ka, String] \ r =
        let EnvMutStateError(ma1) = ma;
        match ma1(e, s, k) {
            case Err(msg) => handleK(msg)
            case Ok(a)    => Ok(a)
        }

    ///
    /// Returns the result of applying `ma` to the state `s`.
    ///
    pub def runEnvMutStateError(_: Region[r], 
                                ma: EnvMutStateError[r, a, env, st, a],
                                e: env,
                                s: st): Result[a, String] \ r =
        let EnvMutStateError(f) = ma;
        let cont = a -> unsafe_cast Ok(a) as \ r;
        f(e, s, cont)


    pub def point(x: a): EnvMutStateError[r, ka, env, st, a] = 
        EnvMutStateError((_, _, k) -> k(x))

    pub def map(f: a -> b \ ef, ma: EnvMutStateError[r, ka, env, st, a]): EnvMutStateError[r, ka, env, st, b] \ ef =
        unsafe_cast (EnvMutStateError((e, s, k) ->
            apply1(ma, e, s, a -> k(unsafe_cast f(a) as \ r))
        )) as \ ef

    pub def ap(mf: EnvMutStateError[r, ka, env, st, a -> b \ ef], ma: EnvMutStateError[r, ka, env, st, a]): EnvMutStateError[r, ka, env, st, b] \ ef =
        unsafe_cast (EnvMutStateError((e, s, k) ->
            apply1(mf, e, s, f ->
                apply1(ma, e, s, a ->
                    k(unsafe_cast f(a) as \ r)))
        )) as \ ef


    pub def flatMap(f: a -> EnvMutStateError[r, ka, env, st, b] \ ef, ma: EnvMutStateError[r, ka, env, st, a]): EnvMutStateError[r, ka, env, st, b] \ ef =
        unsafe_cast (EnvMutStateError((e, s, k) ->
            apply1(ma, e, s, a ->
                apply1(unsafe_cast f(a) as \ r, e, s, b ->
                    k(b)))
        )) as \ ef

    pub def throwError(msg: String): EnvMutStateError[r, ka, env, st, a] = 
        EnvMutStateError((_, _, _) -> unsafe_cast Err(msg) as \ r)

    pub def catchError(ma: EnvMutStateError[r, ka, env, st, a], 
                        hdlr: String -> EnvMutStateError[r, ka, env, st, a] \ ef): EnvMutStateError[r, ka, env, st, a] \ ef= 
        unsafe_cast (EnvMutStateError((e, s, k) -> 
            let EnvMutStateError(ma1) = ma;
                unsafe_cast (match ma1(e, s, k) {
                    case Ok(a)    => Ok(a)
                    case Err(msg) => let EnvMutStateError(hdlr1) = hdlr(msg); hdlr1(e, s, k)
                }) as \ r
        )) as \ ef

    pub def mapError(f: String -> String \ ef, ma: EnvMutStateError[r, ka, env, st, a]): EnvMutStateError[r, ka, env, st, a] \ ef = 
        unsafe_cast (EnvMutStateError((e, s, k) -> 
            apply1WithHandler(ma, e, s, a -> k(a), msg -> unsafe_cast Err(f(msg)) as \ r)
        )) as \ ef


    pub def liftGetter(f: env -> st -> a \ ef): EnvMutStateError[r, ka, env, st, a] \ ef = 
        unsafe_cast (EnvMutStateError((e, s, k) ->
            try {
                let ans = unsafe_cast f(e, s) as \ r;
                k(ans)
            } catch {
                case ex: ##java.lang.Exception => 
                    import java.lang.Throwable.getMessage(): String \ r;
                    Err(getMessage(ex))
            }
        )) as \ ef

    // f should not throw an exception
    pub def liftGetterResult(f: env -> st -> Result[a, err] \ ef): EnvMutStateError[r, ka, env, st, a] \ ef with ToString[err] = 
        unsafe_cast (EnvMutStateError((e, s, k) ->
            match (unsafe_cast f(e, s) as \ r) { 
                case Ok(a)    => k(a)
                case Err(err) => Err(ToString.toString(err))
            } 
        )) as \ ef

    pub def liftSetter!(f: env -> st -> a \ ef): EnvMutStateError[r, ka, env, st, a] \ ef = 
        unsafe_cast (EnvMutStateError((e, s, k) ->
            try {
                let a = unsafe_cast f(e, s) as \ r; 
                k(a)
            } catch {
                case ex: ##java.lang.Exception => 
                    import java.lang.Throwable.getMessage(): String \ r;
                    Err(getMessage(ex))
            }            
        )) as \ ef
        
    // f should not throw an exception
    pub def liftSetterResult!(f: env -> st -> Result[a, err] \ ef): EnvMutStateError[r, ka, env, st, a] \ ef with ToString[err] = 
        unsafe_cast (EnvMutStateError((e, s, k) ->
            match (unsafe_cast f(e, s) as \ r) {
                case Ok(a)    => k(a)
                case Err(err) => Err(ToString.toString(err))
            } 
        )) as \ ef

    pub def liftAction(f: Unit -> a \ ef): EnvMutStateError[r, ka, env, st, a] \ ef = 
        unsafe_cast (EnvMutStateError((_, _, k) ->
            let a = unsafe_cast f() as \ r; 
            k(a)  
        )) as \ ef

    pub def liftActionResult(f: Unit -> Result[a, err] \ ef): EnvMutStateError[r, ka, env, st, a] \ ef with ToString[err] = 
        unsafe_cast (EnvMutStateError((_, _, k) ->
            match (unsafe_cast f() as \ r) {
                case Ok(a)    => k(a)
                case Err(err) => Err(ToString.toString(err))
            } 
        )) as \ ef


    pub def askEnv(): EnvMutStateError[r, ka, env, st, env]  = 
        EnvMutStateError((e, _, k) -> k(e))

    pub def asksEnv(f: env -> a): EnvMutStateError[r, ka, env, st, a]  = 
        EnvMutStateError((e, _, k) -> k(f(e)))

    pub def localEnv(f: env -> env1 \ ef, ma: EnvMutStateError[r, ka, env1, st, a]): EnvMutStateError[r, ka, env, st, a] \ ef = 
        unsafe_cast (EnvMutStateError((e, s, k) -> 
            apply1(ma, unsafe_cast f(e) as \ r, s, a1 -> 
                k(a1))
        )) as \ ef

}
