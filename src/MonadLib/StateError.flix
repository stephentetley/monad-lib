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
    /// Codensity [Pure] State Error monad.
    /// monad in codensity style to be stack safe.
    /// 
    /// `ka` is the continuation answer type
    ///
    pub enum StateError[r: Region, ka: Type, st: Type, a: Type](
        st -> (a -> st -> Result[ka, String] \ r) -> Result[ka, String] \ r)


    instance Functor[StateError[r, ka, st]] {
        pub def map(f: a -> b \ ef, ma: StateError[r, ka, st, a]): StateError[r, ka, st, b] \ ef = 
            MonadLib/StateError.map(f, ma)
    }


    instance Applicative[StateError[r, ka, st]] {
        pub def point(x: a): StateError[r, ka, st, a] =  
            MonadLib/StateError.point(x)

        pub def ap(mf: StateError[r, ka, st, a -> b \ ef], ma: StateError[r, ka, st, a]): StateError[r, ka, st, b] \ ef =
            MonadLib/StateError.ap(mf, ma)
    }

    instance Monad[StateError[r, ka, st]] {
        pub def flatMap(f: a -> StateError[r, ka, st, b] \ ef, ma: StateError[r, ka, st, a]): StateError[r, ka, st, b] \ ef =
            MonadLib/StateError.flatMap(f, ma)
    }

    // Orelse available for Evaluator because state is pure (so can be restored)

    instance MonadLib.MonadOr[StateError[r, ka, st]] { 
        pub def orelse(ma: StateError[r, ka, st, a], mb: StateError[r, ka, st, a]): StateError[r, ka, st, a] =
            MonadLib/StateError.orelse(ma, mb)
    }
    
    instance MonadLib.MonadError[StateError[r, ka, st]] { 
        pub def throwError(msg: String): StateError[r, ka, st, a] =
            MonadLib/StateError.throwError(msg)

        pub def mapError(f: String -> String \ ef, ma: StateError[r, ka, st, a]): StateError[r, ka, st, a] \ ef = 
            MonadLib/StateError.mapError(f, ma)

        pub def catchError(ma: StateError[r, ka, st, a], hdlr: String -> StateError[r, ka, st, a] \ ef): StateError[r, ka, st, a] \ ef = 
            MonadLib/StateError.catchError(ma, hdlr)
    }  

}


namespace MonadLib/StateError {

    use MonadLib.StateError;
    use MonadLib.StateError.{StateError};


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: StateError[r, ka, st, a], 
                s: st, 
                k: a -> st -> Result[ka, String] \ r): Result[ka, String] \ r =
        let StateError(ma1) = ma;
        ma1(s, k)

    def apply1WithHandler(ma: StateError[r, ka, st, a], 
                            s: st, 
                            k: a -> st -> Result[ka, String] \ r, 
                            handleK: String -> Result[ka, String] \ r): Result[ka, String] \ r =
        let StateError(ma1) = ma;
        match ma1(s, k) {
            case Err(msg) => handleK(msg)
            case Ok(a)    => Ok(a)
        }

    ///
    /// Returns the result of applying `ma` to the state `s`.
    ///
    pub def runStateError(_: Region[r], 
                        ma: StateError[r, (st, a), st, a], 
                        s: st): Result[(st, a), String] \ r =
        let StateError(f) = ma;
        let cont = (a, s1) -> unsafe_cast Ok((s1, a)) as \ r;
        f(s, cont)


    pub def point(x: a): StateError[r, ka, st, a] = StateError((s, k) -> k(x, s))

    pub def map(f: a -> b \ ef, ma: StateError[r, ka, st, a]): StateError[r, ka, st, b] \ ef =
        unsafe_cast (StateError((s, k) ->
            apply1(ma, s, (a, s1) -> k(unsafe_cast f(a) as \ r, s1))
        )) as \ ef

    pub def ap(mf: StateError[r, ka, st, a -> b \ ef], ma: StateError[r, ka, st, a]): StateError[r, ka, st, b] \ ef =
        unsafe_cast (StateError((s, k) ->
            apply1(mf, s, (f, s1) ->
                apply1(ma, s1, (a, s2) ->
                    k(unsafe_cast f(a) as \ r, s2)))
        )) as \ ef


    pub def flatMap(f: a -> StateError[r, ka, st, b] \ ef, ma: StateError[r, ka, st, a]): StateError[r, ka, st, b] \ ef =
        unsafe_cast (StateError((s, k) ->
            apply1(ma, s, (a, s1) ->
                apply1(unsafe_cast f(a) as \ r, s1, (b, s2) ->
                    k(b, s2)))
        )) as \ ef


    pub def orelse(ma: StateError[r, ka, st, a], mb: StateError[r, ka, st, a]): StateError[r, ka, st, a] =
        StateError((s, k) ->
            apply1WithHandler(ma, s, a -> k(a), _ -> apply1(mb, s, b -> k(b)))
        )

    pub def throwError(msg: String): StateError[r, ka, st, a] = 
        StateError((_, _) -> unsafe_cast Err(msg) as \ r)

    pub def catchError(ma: StateError[r, ka, st, a], 
                        hdlr: String -> StateError[r, ka, st, a] \ ef): StateError[r, ka, st, a] \ ef= 
        unsafe_cast (StateError((s, k) -> 
            let StateError(ma1) = ma;
                unsafe_cast (match ma1(s, k) {
                    case Ok(a)    => Ok(a)
                    case Err(msg) => let StateError(hdlr1) = hdlr(msg); hdlr1(s, k)
                }) as \ r
        )) as \ ef

    pub def mapError(f: String -> String \ ef, ma: StateError[r, ka, st, a]): StateError[r, ka, st, a] \ ef = 
        unsafe_cast (StateError((s, k) -> 
            apply1WithHandler(ma, s, a -> k(a), msg -> unsafe_cast Err(f(msg)) as \ r)
        )) as \ ef


    pub def update(f: s -> s \ ef): StateError[r, ka, s, Unit] \ ef = 
        unsafe_cast (StateError((s, k) -> 
            k((), unsafe_cast f(s) as \ r)
        )) as \ ef

    pub def put(st: s): StateError[r, ka, s, Unit] = 
        StateError((_, k) -> 
            k((), st)
        )

    pub def get(): StateError[r, ka, s, s] = 
        StateError((s, k) -> 
            k(s, s)
        )

    pub def gets(proj: s -> a \ ef): StateError[r, ka, s, a] \ ef = 
        unsafe_cast (StateError((s, k) -> 
            k(unsafe_cast proj(s) as \ r, s)
        )) as \ ef

}
