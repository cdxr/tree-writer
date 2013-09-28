{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.TreeWriter where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Control.Arrow ( second )

import Data.Tree
import Data.Monoid


-- | @TreeWriter w@ is a specialized version of `WriterT` that outputs a
-- @Forest w@.
newtype TreeWriter w m a = TreeWriter (WriterT (DList (Tree w)) m a)
    deriving (Functor, Applicative, Monad, MonadPlus)

instance MonadTrans (TreeWriter w) where
    lift = TreeWriter . lift


runTreeWriter :: (Monad m) => TreeWriter w m a -> m (a, Forest w)
runTreeWriter (TreeWriter m) = second fromDList `liftM` runWriterT m


-- | Construct a `TreeWriter` computation from a @(result, output)@ pair.
--
-- This is the inverse of `runTreeWriter`.
treeWriter :: (Monad m) => (a, Tree w) -> TreeWriter w m a
treeWriter (a, tw) = TreeWriter $ writer (a, single tw)


-- | Extract the @Forest w@ output from a @TreeWriter w m a@.
execTreeWriter :: (Monad m) => TreeWriter w m a -> m (Forest w)
execTreeWriter = liftM snd . runTreeWriter

-- | Run a @TreeWriter String m a@ computation, printing the resulting
-- @Forest a@ to stdout using `drawForest`.
printTreeWriter :: (MonadIO m) => TreeWriter String m a -> m a
printTreeWriter m = do
    (a, w) <- runTreeWriter m
    liftIO . putStrLn $ drawForest w
    return a


-- | @leaf w@ is a computation that outputs a node with label @w@ and no
-- children.
leaf :: (Monad m) => w -> TreeWriter w m ()
leaf w = treeWriter ((), Node w [])

-- | @node w m@ is a computation that runs computation @m@ to retreive
-- a result @a@ and output @t@. It outputs a node with label @w@ and
-- children @t@, and returns @a@ as its result.
node :: (Monad m) => w -> TreeWriter w m a -> TreeWriter w m a
node w (TreeWriter m) = TreeWriter $ censor (single . Node w . fromDList) m



-- | A simple difference list.
newtype DList a = DList { runDList :: [a] -> [a] }

instance Monoid (DList a) where
    mempty = toDList []
    DList f `mappend` DList g = DList (f . g)

-- | Convert a list to a DList.
toDList :: [a] -> DList a
toDList = DList . (++)

-- | Convert a DList to a list.
fromDList :: DList a -> [a]
fromDList d = runDList d []

-- | A DList consisting of a single item.
single :: a -> DList a
single = toDList . (:[])


