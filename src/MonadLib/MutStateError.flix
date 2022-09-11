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
    /// Mutable State which is equivalent to Reader (without local), and Error 
    /// monad in codensity style to be stack safe.
    /// 
    /// `ka` is the continuation answer type
    ///
    pub enum MutStateError[r: Region, ka: Type, st: Type, a: Type](
        st -> (a -> Result[ka, String] \ r) -> Result[ka, String] \ r)


    instance Functor[MutStateError[r, ka, st]] {
        pub def map(f: a -> b \ ef, ma: MutStateError[r, ka, st, a]): MutStateError[r, ka, st, b] \ ef = 
            MonadLib/MutStateError.map(f, ma)
    }


    instance Applicative[MutStateError[r, ka, st]] {
        pub def point(x: a): MutStateError[r, ka, st, a] =  
            MonadLib/MutStateError.point(x)

        pub def ap(mf: MutStateError[r, ka, st, a -> b \ ef], ma: MutStateError[r, ka, st, a]): MutStateError[r, ka, st, b] \ ef =
            MonadLib/MutStateError.ap(mf, ma)
    }

    instance Monad[MutStateError[r, ka, st]] {
        pub def flatMap(f: a -> MutStateError[r, ka, st, b] \ ef, ma: MutStateError[r, ka, st, a]): MutStateError[r, ka, st, b] \ ef =
            MonadLib/MutStateError.flatMap(f, ma)
    }

    /// No `MonadOr` for MutStateError because updated state cannot be unwound.
    
    instance MonadLib.MonadError[MutStateError[r, ka, st]] { 
        pub def throwError(msg: String): MutStateError[r, ka, st, a] =
            MonadLib/MutStateError.throwError(msg)

        pub def modifyError(f: String -> String \ ef, ma: MutStateError[r, ka, st, a]): MutStateError[r, ka, st, a] \ ef = 
            MonadLib/MutStateError.modifyError(f, ma)

        pub def catchError(ma: MutStateError[r, ka, st, a], hdlr: String -> MutStateError[r, ka, st, a] \ ef): MutStateError[r, ka, st, a] \ ef = 
            MonadLib/MutStateError.catchError(ma, hdlr)
    }  

}


namespace MonadLib/MutStateError {

    use MonadLib.MutStateError;
    use MonadLib.MutStateError.{MutStateError};


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: MutStateError[r, ka, st, a], 
                s: st, 
                k: a -> Result[ka, String] \ r): Result[ka, String] \ r =
        let MutStateError(ma1) = ma;
        ma1(s, k)

    def apply1WithHandler(ma: MutStateError[r, ka, st, a], 
                            s: st, 
                            k: a -> Result[ka, String] \ r, 
                            handleK: String -> Result[ka, String] \ r): Result[ka, String] \ r =
        let MutStateError(ma1) = ma;
        match ma1(s, k) {
            case Err(msg) => handleK(msg)
            case Ok(a)    => Ok(a)
        }

    ///
    /// Returns the result of applying `ma` to the state `s`.
    ///
    pub def runMutStateError(_: Region[r], 
                        ma: MutStateError[r, a, st, a], 
                        s: st): Result[a, String] \ r =
        let MutStateError(f) = ma;
        let cont = a -> Ok(a) as \ r;
        f(s, cont)


    pub def point(x: a): MutStateError[r, ka, st, a] = MutStateError((_, k) -> k(x))

    pub def map(f: a -> b \ ef, ma: MutStateError[r, ka, st, a]): MutStateError[r, ka, st, b] \ ef =
        MutStateError((s, k) ->
            apply1(ma, s, a -> k(f(a) as \ r))
        ) as \ ef

    pub def ap(mf: MutStateError[r, ka, st, a -> b \ ef], ma: MutStateError[r, ka, st, a]): MutStateError[r, ka, st, b] \ ef =
        MutStateError((s, k) ->
            apply1(mf, s, f ->
                apply1(ma, s, a ->
                    k(f(a) as \ r)))
        ) as \ ef


    pub def flatMap(f: a -> MutStateError[r, ka, st, b] \ ef, ma: MutStateError[r, ka, st, a]): MutStateError[r, ka, st, b] \ ef =
        MutStateError((s, k) ->
            apply1(ma, s, a ->
                apply1(f(a) as \ r, s, b ->
                    k(b)))
        ) as \ ef

    pub def throwError(msg: String): MutStateError[r, ka, st, a] = 
        MutStateError((_, _) -> Err(msg) as \ r)

    pub def catchError(ma: MutStateError[r, ka, st, a], 
                        hdlr: String -> MutStateError[r, ka, st, a] \ ef): MutStateError[r, ka, st, a] \ ef= 
        MutStateError((s, k) -> 
            let MutStateError(ma1) = ma;
                match ma1(s, k) {
                    case Ok(a)    => Ok(a)
                    case Err(msg) => let MutStateError(hdlr1) = hdlr(msg); hdlr1(s, k)
                } as \ r
        ) as \ ef



    pub def modifyError(f: String -> String \ ef, ma: MutStateError[r, ka, st, a]): MutStateError[r, ka, st, a] \ ef = 
        MutStateError((s, k) -> 
            apply1WithHandler(ma, s, a -> k(a), msg -> Err(f(msg)) as \ r)
        ) as \ ef


    pub def liftGetter(f: st -> a \ ef): MutStateError[r, ka, st, a] \ ef = 
        MutStateError((s, k) ->
            try {
                let ans = f(s) as \ r;
                k(ans)
            } catch {
                case ex: ##java.lang.Exception => 
                    import java.lang.Throwable.getMessage(): String \ r;
                    Err(getMessage(ex))
            }            
        ) as \ ef

    // f should not throw an exception
    pub def liftGetterResult(f: st -> Result[a, err] \ ef): MutStateError[r, ka, st, a] \ ef with ToString[err] = 
        MutStateError((s, k) ->
            match f(s) { 
                case Ok(a)  => k(a)
                case Err(e) => Err(ToString.toString(e))
            } as \ r
        ) as \ ef

    pub def liftSetter!(f: st -> a \ ef): MutStateError[r, ka, st, a] \ ef = 
        MutStateError((s, k) ->
            try {
                let a = f(s) as \ r; 
                k(a)
            } catch {
                case ex: ##java.lang.Exception => 
                    import java.lang.Throwable.getMessage(): String \ r;
                    Err(getMessage(ex))
            }            
        ) as \ ef
        
    // f should not throw an exception
    pub def liftSetterResult!(f: st -> Result[a, err] \ ef): MutStateError[r, ka, st, a] \ ef with ToString[err] = 
        MutStateError((s, k) ->
            match f(s) {
                case Ok(a)  => k(a)
                case Err(e) => Err(ToString.toString(e))
            } as \ r
        ) as \ ef

    pub def liftAction(f: Unit -> a \ ef): MutStateError[r, ka, st, a] \ ef = 
        MutStateError((_, k) ->
            let a = f() as \ r; 
            k(a)  
        ) as \ ef

    pub def liftActionResult(f: Unit -> Result[a, err] \ ef): MutStateError[r, ka, st, a] \ ef with ToString[err] = 
        MutStateError((_, k) ->
            match f() {
                case Ok(a)  => k(a)
                case Err(e) => Err(ToString.toString(e))
            } as \ r
        ) as \ ef

}
