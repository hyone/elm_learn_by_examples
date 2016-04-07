-- http://elm-lang.org/examples/binary-tree

module BinaryTree where

import Text

type Tree a = Empty | Node a (Tree a) (Tree a)

empty : Tree a
empty = Empty

singleton : a -> Tree a
singleton v = Node v Empty Empty

insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
  case tree of
    Empty ->
      singleton x
    Node y left right ->
      if x > y then
        Node y left (insert x right)
      else if x < y then
        Node y (insert x left) right
      else
        tree

fromList : List comparable -> Tree comparable
fromList xs = List.foldl insert empty xs

depth : Tree a -> Int
depth tree =
  case tree of
    Empty -> 0
    Node v left right ->
      1 + max (depth left) (depth right)

map : (a -> b) -> Tree a -> Tree b
map f tree =
  case tree of
    Empty -> Empty
    Node v left right ->
      Node (f v) (map f left) (map f right)


t1 = fromList [1,2,3]
t2 = fromList [2,1,3]

{-----------------------------------------------------------------
   Exercises:
 -----------------------------------------------------------------}

-- 1)
sum : Tree number -> number
sum tree =
  case tree of
    Empty -> 0
    Node v left right ->
      v + (sum left) + (sum right)

-- 2)
flatten : Tree a -> List a
flatten tree =
  case tree of
    Empty -> []
    Node v left right ->
      flatten left ++ (v :: flatten right)

-- 3)
isElement : a -> Tree a -> Bool
isElement x tree =
  case tree of
    Empty -> False
    Node v left right ->
      if x == v then
        True
      else
        isElement x left || isElement x right

-- 4)
fold : (a -> b -> b) -> b -> Tree a -> b
fold f x tree =
  case tree of
    Empty -> x
    Node v left right ->
      fold f (f v (fold f x left)) right

-- 5)
sum' : Tree number -> number
sum' = fold (+) 0

flatten' : Tree a -> List a
flatten' = fold (::) []

isElement' : a -> Tree a -> Bool
isElement' x tree = map ((==) x) tree |> fold (||) False

-- 6)
-- Can't use "fold" to implemnt "map" and "depth"

-- 7)
foldPreOrder : (a -> b -> b) -> b -> Tree a -> b
foldPreOrder f x tree =
  case tree of
    Empty -> x
    Node v left right ->
      f v x
      |> flip (foldPreOrder f) left
      |> flip (foldPreOrder f) right

foldInOrder : (a -> b -> b) -> b -> Tree a -> b
foldInOrder f x tree =
  case tree of
    Empty -> x
    Node v left right ->
      foldInOrder f x left
      |> f v
      |> flip (foldInOrder f) right

foldPostOrder : (a -> b -> b) -> b -> Tree a  -> b
foldPostOrder f x tree =
  case tree of
    Empty -> x
    Node v left right ->
      foldPostOrder f x left
      |> flip (foldPostOrder f) right
      |> f v

{-
  > import BinaryTree exposing (..)

  > t = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 (Node 6 Empty Empty) (Node 7 Empty Empty))
  Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 (Node 6 Empty Empty) (Node 7 Empty Empty))
      : BinaryTree.Tree number

  > flattenPreOrder = foldPreOrder (::) []
  <function> : BinaryTree.Tree a -> List a
  > flattenInOrder = foldInOrder (::) []
  <function> : BinaryTree.Tree a -> List a
  > flattenPostOrder = foldPostOrder (::) []
  <function> : BinaryTree.Tree a -> List a

  > flattenPreOrder t
  [7,6,3,5,4,2,1] : List number
  > flattenInOrder t
  [7,3,6,1,5,2,4] : List number
  > flattenPostOrder t
  [1,3,7,6,2,5,4] : List number
-}
