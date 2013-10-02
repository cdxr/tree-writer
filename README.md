# tree-writer

The module `Control.Monad.TreeWriter`, defines the type `TreeWriter w m a`, a
monad transformer isomorphic to `WriterT (Data.Tree.Tree w) m a`.

## motivation

This very simple monad transformer is the result of a brief experiment I
conducted while exploring possible implementations of a logging monad
transformer. As it is based on the WriterT monad transformer, its output is
constructed lazily. I am currently exploring a strict pipes-based solution for
my logger, but this module might prove interesting in unexpected ways.

## overview

This overview assumes familiarity with `Control.Monad.Trans.Writer` from
`transformers` and `Data.Tree` from `containers`.

Transforming a monad with `TreeWriter w` results in a monad that outputs a
`Forest w`.

```haskell
runTreeWriter :: (Monad m) => TreeWriter w m a -> m (a, Forest w)
```

The `TreeWriter` analogue to `tell` is `leaf`:

```haskell
leaf :: (Monad m) => w -> TreeWriter w m ()
```

`leaf a` is like WriterT's `tell [Node a []]`. Note that it is not necessary
for the parameter to be a monoid, because `[a]` is a monoid for any `a`.

The original motivation for this module is the following function:

```haskell
node :: (Monad m) => w -> TreeWriter w m a -> TreeWriter w m a
```

`node a m` outputs a node with label `a` like `leaf a` does, but the node's
subforest is the output of `m`.

```haskell
example :: (Monad m) => TreeWriter Int m String
example = do
    leaf 0
    node 10 $ do
        leaf 20
        leaf 30
        return "hello"

-- outputs ("hello", [Node 0 [], Node 10 [Node 20 [], Node 30 []]])
main = print . runIdentity $ runTreeWriter example
```
