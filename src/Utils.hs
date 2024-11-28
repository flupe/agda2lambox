-- | Language-independent utilities.
module Utils where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

-- ** basics
enumerate0, enumerate1 :: [a] -> [(Int, a)]
enumerate0 = zip [0..]
enumerate1 = zip [1..]

(/\), (\/) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(p /\ q) x = p x && q x
(p \/ q) x = p x || q x

-- ** printing
report :: MonadIO m => String -> m ()
report s = liftIO $ putStrLn s
