{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Algebraic
       ( -- * 'MonadIO'
         MonadIO(..),
         -- * 'MonadUIO'
         MonadUIO(..), liftIOSafely,
         -- * 'MonadThrow'
         MonadThrow(..), catch, try,
         -- * 'MonadError'
         MonadError(..), catchError,
         -- * 'MonadReader'
         MonadReader(..), local,
         -- * 'MonadState'
         MonadState(..),
         -- * 'MonadWriter'
         MonadWriter(..), listen)
       where

import Control.Monad.Catch (MonadThrow(..), Exception, SomeException, fromException)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Writer.Strict (WriterT(..))
import Control.Monad.Catch.Pure (CatchT(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (MonadState(..))
import UnexceptionalIO (UIO, fromIO)

--------------------------------------------------------------------------------
-- | The class of 'Monad's that perform I/O without throwing synchronous
-- exceptions.
class MonadUIO m where
  liftUIO :: UIO a -> m a

instance MonadUIO UIO where
  liftUIO = id

-- | Lift an 'IO' computation into @m@. If the given 'IO' computation throws
-- /synchronous/ exceptions, these will be explicitly thrown under the
-- 'MonadThrow' effect.
liftIOSafely :: (MonadThrow m,MonadUIO m)
             => IO a -> m a
liftIOSafely io = liftUIO (fromIO io) >>= either throwM return

--------------------------------------------------------------------------------
catch :: (Exception e, MonadThrow m) => CatchT m a -> (e -> m a) -> m a
catch (CatchT m) c =
  do ea <- m
     case ea of
       Left e ->
         case fromException e of
           Just e' -> c e'
           Nothing -> throwM e
       Right a -> return a

try :: CatchT m a -> m (Either SomeException a)
try = runCatchT

--------------------------------------------------------------------------------
class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a

catchError :: Monad m => ExceptT e m a -> (e -> m a) -> m a
catchError m k = runExceptT m >>= either k return

--------------------------------------------------------------------------------
class Monad m => MonadReader r m  | m -> r where
  ask :: m r
  ask = reader id
  reader :: (r -> a) -> m a
  reader f = fmap f ask

local :: (MonadReader r m) => (r -> r) -> ReaderT r m a -> m a
local f m = runReaderT m =<< fmap f ask

--------------------------------------------------------------------------------
class (Monoid w,Monad m) => MonadWriter w m  | m -> w where
  -- | @'writer' (a,w)@ embeds a simple writer action.
  writer :: (a,w) -> m a
  writer ~(a,w) =
    do tell w
       return a
  -- | @'tell' w@ is an action that produces the output @w@.
  tell :: w -> m ()
  tell w = writer ((),w)

listen :: MonadWriter w m => WriterT w m a -> m (a,w)
listen m =
  do (a,w) <- runWriterT m
     tell w
     return (a,w)
