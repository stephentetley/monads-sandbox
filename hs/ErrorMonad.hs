{-# LANGUAGE Rank2Types #-}

module ErrorMonad where



-- Existential envy!

data Result a e = Ok a | Err e
    deriving (Eq, Ord, Show)

newtype Error a = Error { getError :: forall ka. (a -> Result ka String) -> Result ka String }

apply1 :: Error a -> (a -> Result ka String) -> Result ka String
apply1 ma k = let ma1 = getError ma in ma1(k)

runError :: Error ans -> Result ans String
runError ma = let ma1 = getError ma in ma1 (\a -> Ok a)
        


instance Functor Error where
    fmap f ma = Error(\k -> apply1 ma $ \v1 -> k (f v1))

instance Applicative Error where
    pure a = Error(\k -> k a)

    mf <*> ma = 
        Error(\k -> 
            apply1 mf $ \f1 ->
                apply1 ma $ \a1 ->
                    k (f1 a1)
        )

instance Monad Error where
    ma >>= f = 
        Error(\k ->
            apply1 ma $ \a1 ->
                apply1 (f a1) $ \b1 ->
                    k b1
        )

throwError1 :: String -> Error a
throwError1 msg = Error(\_ -> Err(msg))

catchError1 :: Error a -> (String -> Error a) -> Error a
catchError1 ma handler = 
    Error(\k -> case runError(ma)  of
                    Ok b1  -> k b1 
                    Err e  -> apply1 (handler e) $ \b1 -> k b1
                )

reifyError1 :: Error ans -> Error (Result ans String)
reifyError1 ma = 
    Error(\k -> let ans = runError ma in k ans)

-- `reifyError` is not very useful if we have `IfThenElse`
class Monad m => MonadError m where
    throwError :: String -> m a
    catchError :: m a -> (String -> m a) -> m a
    reifyError :: m a -> m (Result a String)

instance MonadError Error where
    throwError msg = throwError1 msg
    catchError ma handler = catchError1 ma handler
    reifyError ma = reifyError1 ma

class MonadError m => IfThenElse m where
    ifThenElse :: m a -> (a -> m b) -> (String -> m b) -> m b

instance IfThenElse Error where
    ifThenElse ma sk fk = ifThenElse1 ma sk fk

ifThenElse1 :: Error a -> (a -> Error b) -> (String -> Error b) -> Error b
ifThenElse1 ma sk fk = 
    Error(\k -> case runError(ma)  of
                    Ok a1  -> apply1 (sk a1) $ \b1 -> k b1
                    Err e  -> apply1 (fk e) $ \b1 -> k b1
                )


class MonadError m => MonadSuccess m where
    whenOk :: m a -> (a -> m b) -> m b


instance MonadSuccess Error where
    whenOk ma sk = success1 ma sk

-- This has the same type as bind...
success1 :: Error a -> (a -> Error b) -> Error b
success1 ma sk = 
    Error(\k -> case runError(ma)  of
                    Ok a1  -> apply1 (sk a1) $ \b1 -> k b1
                    Err e  -> Err e
                )

{-
foldLeftM :: IfThenElse m => (b -> a -> m b) -> b -> [a] -> m b
foldLeftM f s l =
    let loop ll acc = case ll of
                        []     -> pure acc
                        x : xs -> ifThenElse (f acc x) (\ans -> loop xs ans) (\e -> throwError e)
    in loop l s
-}

foldLeftM :: MonadSuccess m => (b -> a -> m b) -> b -> [a] -> m b
foldLeftM f s l =
    let loop ll acc = case ll of
                        []     -> pure acc
                        x : xs -> whenOk (f acc x) (\ans -> loop xs ans)
    in loop l s

traverseCps :: IfThenElse m => (a -> m b) -> [a] -> m [b]
traverseCps f l = 
    let loop ll k = case ll of
                        []     -> k []
                        x : xs -> ifThenElse (f x) (\a1 -> loop xs (\ks -> k (a1 : ks))) (\e -> throwError e)
    in loop l pure


-- Bind must be stack safe...
traverseCps1 :: Monad m => (a -> m b) -> [a] -> m [b]
traverseCps1 f l = 
    let loop ll k = case ll of
                        []     -> k []
                        x : xs -> (f x) >>= (\a1 -> loop xs (\ks -> k (a1 : ks)))
    in loop l pure    

fl01 = runError $ foldLeftM (\acc x -> if x < 100 then pure (x + acc) else throwError "too big") 0 [0 .. 99] 
fl02 = runError $ foldLeftM (\acc x -> if x < 100 then pure (x + acc) else throwError "too big") 0 [0 .. 101] 
t01 = runError $ traverseCps (\x -> if x < 100 then pure x else throwError "too big") [0 .. 99]
t02 = runError $ traverseCps (\x -> if x < 100 then pure x else throwError "too big") [0 .. 101]
t01' = runError $ traverseCps1 (\x -> if x < 100 then pure x else throwError "too big") [0 .. 99]
t02' = runError $ traverseCps1 (\x -> if x < 100 then pure x else throwError "too big") [0 .. 101]