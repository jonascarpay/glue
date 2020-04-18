module NonGL where

import Control.Monad

whenM :: Monad m => m Bool -> m () -> m ()
whenM check act = check >>= flip when act

while :: Monad m => m Bool -> m () -> m ()
while check act = go
  where
    go = do
      cond <- check
      when cond $
        act >> go

