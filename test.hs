import           Control.Applicative ((<$>))
import           Control.Monad       (foldM, sequence)
import           Data.Monoid         (Monoid, mconcat)

import           Control.Monad.Loops (unfoldWhileM)



{- doWhile :: (Monad m, Monoid a) => (a -> Bool) -> m a -> m a -}
doWhile pr go = (mconcat . takeWhile pr) <$> sequence $ repeat go

