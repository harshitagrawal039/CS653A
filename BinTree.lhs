#!/usr/bin/env runhaskell.

This is a literate Haskell file and can be compiled/executed directly.
Only the lines that begin with a bird track ('>') is relevant, the
rest is treated as comments. The Haskell compiler can compile these
files directly. What is good about literate Haskell source is that
they can indeed be converted to a document using tools like pandoc.

> module BinTree where

A formal way of defining a binary tree is through a recursive
definition. A binary tree is either:

1. An empty tree

or

2. A node which has a label (captured by the type variable a) and two
subtrees, each of which is a binary tree.

This more or less translates directly to the following Haskell
definition.

> data BinTree a = NullTree
>                | Node (BinTree a) a (BinTree a)
>		deriving Show


The above definition results in the following:

1. A new (one parameter) type called BinTree.

2. Two constructors, NullTree and Node, for BinTree.

Functions on binary tree can be defined using pattern matching on
these constructors. Here, are few examples.

> -- | checks whether a given binary tree is empty
> null :: BinTree a -> Bool
> null NullTree = True
> null _        = False

> -- | The empty tree
> empty :: BinTree a
> empty = NullTree


> -- | A tree with precisely one node
> singleton :: a -> BinTree a
> singleton a = Node empty a empty


Let us look at the inorder traversal of the binary tree.

> -- | Gives the nodes of the tree according to inorder traversal
> inorder :: BinTree a -> [a]
> inorder NullTree = []
> inorder (Node lt a rt) = inorder lt ++ a : inorder rt

Exercise:

Give function for pre-order and post order traversal.

Now let us define an important operation of rotation. These,
operations are used often to balance trees. We define the clock wise
rotation. The essential idea is captured by this picture.



                a                        b
               / \                      / \
              /   \                    /   \
             /     \    --------->    /     \
            b       t               t1       a
           / \                              / \
          /   \                            /   \
         t1   t2                          t2    t


> -- | Rotates a given tree clockwise at the root.
> clockwise :: BinTree a -> BinTree a
> clockwise (Node  (Node t1 b t2) a t) = Node t1 b (Node t2 a t)
> clockwise t                          = t

As an exercise, define the the antiClockwise function
