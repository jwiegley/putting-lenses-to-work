{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main ( Record, HasRecord(..)
            , digits
            , main
            ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State
import           Data.Char
import           Data.Data
import           Data.Data.Lens
import           Data.Function
import           Data.List
import           Data.Maybe (isJust)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Test.Hspec

default (Int)

data Record = Record
  { _field1 :: Int
  , _field2 :: Int
  }
  deriving (Eq, Show)

makeClassy ''Record

data ADT = Alpha Int Int
         | Beta Record
         | Gamma String
  deriving (Eq, Show)

makePrisms ''ADT

digits :: Iso' Int [Int]
digits =
  iso (map (read :: String -> Int) . sequence . (:[]) . show)
      ((read :: String -> Int) . concatMap show)

my_1 :: Lens' (Integer, Integer) Int
my_1 f (p1, p2) =
  (\n -> (toInteger n, p2)) <$> f (fromIntegral p1)

my_1' :: Functor f
      => (Int -> f Int)
      -> (Integer, Integer)
      -> f (Integer, Integer)
my_1' f (p1, p2) =
  (\n -> (toInteger n, p2)) <$> f (fromIntegral p1)

newtype Bar = Bar
  { _baz :: Int
  } deriving (Eq, Show)
makeLenses ''Bar

newtype Foo = Foo
  { _bar :: Bar
  } deriving (Eq, Show)
makeLenses ''Foo

newtype Top = Top
  { _foo :: Foo
  } deriving (Eq, Show)
makeLenses ''Top

foobar :: Traversal [a] (Maybe [a]) a (Maybe a)
foobar f xs = fmap sequence (xs & traverse %%~ f)

example :: (a -> Maybe a) -> [a] -> Maybe [a]
example f xs = xs & partsOf (traversed.filtered (isJust . f))._head %%~ f

{-----------------------------------------------------------------------------}

infixr 0 ==>
(==>) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(==>) = shouldBe

infixr 0 /=>
(/=>) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(/=>) = shouldNotBe

infixr 0 !!>
(!!>) :: (HasCallStack, Exception e) => a -> Selector e -> Expectation
v !!> s = pure v `shouldThrow` s

bag :: Ord a => [a] -> Set a
bag = S.fromList

alist :: Ord k => [(k, a)] -> Map k a
alist = M.fromList

{-----------------------------------------------------------------------------}

main :: IO ()
main = hspec $ parallel $ do
  describe "Lens" $ do
    describe "Tuple" $ do
      describe "view" $ do
        it "operator" $
          (1,2,3) ^. _2
            ==> 2

        it "function" $
          view _2 (1,2,3)
            ==> 2

      describe "set" $ do
        it "operator" $
          (1,2,3) & _2 .~ 20
            ==> (1,20,3)

        it "function" $
          set _2 20 (1,2,3)
            ==> (1,20,3)

      it "both" $
        (1,2) ^.. both
          ==> [1,2]

      it "each" $
        (1,2,3) ^.. each
          ==> [1,2,3]

      it "alongside" $
        ((1,2), (3,4)) ^. alongside _2 _1
          ==> (2,3)

    describe "records" $ do
      it "view" $
        Record 20 30 ^. field1
          ==> 20

      it "set" $
        Record 20 30 & field1 .~ 1
          ==> Record 1 30

      it "over" $
        Record 20 30 & field1 %~ mod 5
          ==> Record 5 30

      it "overA" $
        Record 20 30 & field1
          %%~ (pure :: a -> Identity a)
          ==> pure (Record 20 30)

      it "matches" $
        let v = Top (Foo (Bar 100)) in
        v & foo.bar.baz +~ 1
          ==> let f = _foo v
                  b = _bar f
                  z = _baz b in
              v { _foo = f {
                    _bar = b {
                      _baz = z + 1 } } }

    describe "Wrapped" $ do
      it "view" $
        Sum 5 ^. _Wrapped
          ==> 5

      it "set" $
        Sum 5 & _Wrapped +~ 10
          ==> Sum 15

      it "over" $
        Sum 30 & _Wrapped %~ (+10)
          ==> Sum 40

    describe "custom" $ do
      it "my_1" $
        (100, 200) ^. my_1
          ==> 100

      it "my_1-fails" $
        (10000000000000000000000000000000000, 200)
          ^. my_1
          ==> 4003012203950112768

      it "my_1'" $
        (100, 200) ^. my_1'
          ==> 100

  describe "Prism" $ do
    describe "Either" $ do
      describe "preview" $ do
        it "present" $
          Left 10 ^? _Left
            ==> Just 10

        it "absent" $
          (Right 10 :: Either Int Int) ^? _Left
            ==> Nothing

      describe "setter" $ do
        it "present" $
          (Left 10 :: Either Int Int) & _Left .~ 2
            ==> Left 2

        it "absent" $
          (Left 10 :: Either Int Int) & _Right .~ 2
            ==> Left 10

      describe "over" $ do
        it "present" $
          (Left 10 :: Either Int Int)
            & _Left %~ even
            ==> Left True

        it "absent" $
          (Left 10 :: Either Int Int)
            & _Right %~ even
            ==> Left 10

        it "failover" $
          (Left 10 :: Either Int Int)
            & failover _Right even
            ==> (mzero :: Maybe (Either Int Bool))

      it "has" $
        has _Left (Left 5)
          ==> True

      it "isn't" $
        isn't _Right (Left 5)
          ==> True

      it "review" $
        _Alpha # (30,40)
          ==> Alpha 30 40

      it "matches" $
        let v = Beta (Record 10 20) in
        v & _Beta.field1 +~ 1
          ==> case v of
            Beta z ->
              Beta (z { _field1 = _field1 z + 1 })
            _ -> v

    describe "ADTs" $ do
      describe "preview" $ do
        it "present" $
          Alpha 10 20 ^? _Alpha._2
            ==> Just 20

        it "absent" $
          Gamma "hello" ^? _Alpha._2
            ==> Nothing

      describe "setter" $ do
        it "present" $
          Alpha 10 20 & _Alpha._2 .~ 2
            ==> Alpha 10 2

        it "absent" $
          Alpha 10 20 & _Beta.field1 .~ 2
            ==> Alpha 10 20

      it "has" $
        has _Beta (Alpha 1 2)
          ==> False

      it "review" $
        _Alpha # (30,40)
          ==> Alpha 30 40

  describe "Traversal" $ do
    describe "List" $ do
      it "toListOf" $
        [1,2,3,4] ^.. traverse
          ==> [1,2,3,4]

      it "allOf" $
        allOf (traverse._2) even
          [(1, 10), (2, 12)]
          ==> True

      it "monoid" $
        [1,2,3,4] ^. traverse.to Sum
          ==> Sum 10

      it "preview" $
        [1,2,3] ^? ix 1
          ==> Just 2

      it "has" $
        has (ix 1) [1,2,3]
          ==> True

      it "set" $
        [1,2,3] & ix 1 .~ 20
          ==> [1,20,3]

      describe "only" $ do
        it "success" $
          5 ^? only 5
            ==> Just ()

        it "failure" $
          5 ^? only 4
            ==> Nothing

        it "has-filtered" $
          has (traverse.filtered (== 5)) [1,2,3,4]
            ==> False

        it "has-only" $
          has (traverse.only 5) [1,2,3,4]
            ==> False

      describe "singular" $ do
        it "_head-alone" $
          [1,2,3] ^? _head
            ==> Just 1

        it "_head" $
          [1,2,3] ^. singular _head
            ==> 1

    describe "Set" $
      describe "contains" $ do
        it "view" $
          bag [1, 2, 3] ^. contains 2
            ==> True

        it "set" $
          bag [1, 2, 3] & contains 2 .~ False
            ==> bag [1, 3]

    describe "computations" $
      describe "digits" $ do
        it "preview" $
          31415926 ^? digits.ix 2
            ==> Just 4

        it "set" $
          31415926 & digits.ix 2 .~ 8
            ==> 31815926

        it "set-flexible" $
          31415926 & digits.ix 2 .~ 99
            ==> 319915926

    describe "similar" $ do
      it "like-fmap" $
        [1,2,3,4] & mapped +~ 1
          ==> [2,3,4,5]

      it "still-like-fmap" $
        ([1,2,3,4], 10) & mapped +~ 1
          ==> ([1,2,3,4], 11)

      it "not-like-fmap" $
        ([1,2,3,4], 10) & _1.mapped +~ 1
          ==> ([2,3,4,5], 10)

      it "folded" $
        [1,2,3,4] ^. folded.to Sum
          ==> Sum 10

      it "minimumOf" $
        minimumOf (each.field2)
          (Record 1 2,Record 3 4,Record 5 6)
          ==> Just 2

      it "sumOf" $
        sumOf (each.field2)
          (Record 1 2,Record 3 4,Record 5 6)
          ==> 12

      it "sequenceAOf" $
        sequenceAOf _2 (1,"foo",2)
          ==> [(1,'f',2),(1,'o',2),(1,'o',2)]

  describe "Map" $ do
    describe "at" $ do
      describe "view" $ do
        it "present" $
          alist [(1,"x"), (2,"y")] ^. at 1
            ==> Just "x"

        it "absent" $
          alist [(1,"x"), (2,"y")] ^. at 3
            ==> Nothing

      describe "non" $ do
        it "present" $
          alist [(1,"x"), (2,"y")]
            ^. at 2.non "z"
            ==> "y"

        it "absent" $
          alist [(1,"x"), (2,"y")]
            ^. at 3.non "z"
            ==> "z"

      describe "set" $ do
        it "positive" $
          alist [(1,"x"), (2,"y")]
            & at 1 .~ Just "z"
            ==> alist [(1,"z"), (2,"y")]

        it "negative" $
          alist [(1,"x"), (2,"y")]
            & at 1 .~ Nothing
            ==> alist [(2,"y")]

        it "sans" $
          alist [(1,"x"), (2,"y")] & sans 1
            ==> alist [(2,"y")]

    describe "ix" $ do
      describe "view" $ do
        it "present" $
          alist [(1,"x"), (2,"y")] ^? ix 1
            ==> Just "x"

        it "demand" $
          alist [(1,"x"), (2,"y")] ^?! ix 1
            ==> "x"

        it "absent" $
          alist [(1,"x"), (2,"y")] ^? ix 3
            ==> Nothing

        it "failing" $
          alist [(1,"x"), (2,"y")]
            ^? failing (ix 3) (ix 1)
            ==> Just "x"

        it "like" $
          alist [(1,"x"), (2,"y")]
            ^?! failing (ix 3) (like "z")
            ==> "z"

      describe "set" $ do
        it "present" $
          alist [(1,"x"), (2,"y")] & ix 1 .~ "z"
            ==> alist [(1,"z"), (2,"y")]

        it "absent" $
          alist [(1,"x"), (2,"y")] & ix 3 .~ "z"
            ==> alist [(1,"x"), (2,"y")]

    describe "traverse" $ do
      it "toListOf" $
        alist [(1,"x"), (2,"y")] ^.. traverse
          ==> ["x","y"]

      it "withIndex" $
        alist [(1,"x"), (2,"y")]
          ^.. traversed.withIndex
          ==> [(0,"x"),(1,"y")]

    describe "itraversed" $
      describe "indices" $ do
        it "toListOf" $
          alist [(1,"x"), (2,"y"), (4,"z")]
            ^.. itraversed.indices even
            ==> ["y", "z"]

        it "withIndex" $
          alist [(1,"x"), (2,"y")]
            ^.. itraversed.withIndex
            ==> [(1,"x"),(2,"y")]

        it "indices" $
          alist [(1,"x"), (2,"y"), (4,"z")]
            & itraversed.indices even .~ "w"
            ==> alist [(1,"x"),(2,"w"),(4,"w")]

  describe "State" $ do
    it "use" $
      use _1 `evalState` (10, 20)
        ==> 10

    it "uses" $
      uses _1 negate `evalState` (10, 20)
        ==> -10

    it "preuse" $
      preuse (ix 1) `evalState` [1, 2, 3, 4]
        ==> Just 2

    it "preuses" $
      preuses (ix 1) negate
        `evalState` [1, 2, 3, 4]
        ==> Just (-2)

    it "set" $
      (ix 1 .= 5) `execState` [1, 2, 3, 4]
        ==> [1, 5, 3, 4]

    it "over" $
      (ix 1 %= negate)
        `execState` [1, 2, 3, 4]
        ==> [1, -2, 3, 4]

    it "setM" $
      (ix 1 <~ pure 5) `execState` [1, 2, 3, 4]
        ==> [1, 5, 3, 4]

    it "zoom" $
      zoom _1 (_2 .= 4) `execState` ((1, 2), 3)
        ==> ((1, 4), 3)

    it "multi-set-plain" $
      ((1,2,3) & _1 .~ 10
               & _2 .~ 20
               & _3 .~ 30)
        ==> (10,20,30)

    it "multi-set" $
      ((1,2,3) &~ do _1 .= 10
                     _2 .= 20
                     _3 .= 30)
        ==> (10,20,30)

  describe "Advanced" $ do
    describe "partsOf" $ do
      it "indices" $
        [2,4,1,5,3,6]
          & partsOf (traversed.indices odd)
          %~ reverse
          ==> [2,6,1,5,3,4]

      it "filtered" $
        [2,4,1,5,3,6]
          & partsOf (traverse.filtered (< 4))
          %~ reverse.sort
          ==> [3,4,2,5,1,6]

      it "each" $
        (3,1,2,4,5) & partsOf each %~ sort
          ==> (1,2,3,4,5)

      it "set" $
        "Hello, World"
          & partsOf (traverse.filtered isAlpha)
          .~ "Howdy!!!"
          ==> "Howdy, !!!ld"

      it "multiple" $
        "00:00:00"
          & partsOf (itraversed.
                     indices (>= 3).
                     filtered (== '0'))
          .~ "1234"
          ==> "00:12:34"

    describe "view-patterns" $
      it "lambda" $
        (\(view _2 -> Left x) -> x) (10, Left 20)
          ==> 20

    describe "biplate" $ do
      it "strings" $
        ((("foo", "bar"), "!", 2 :: Int, ())
           ^.. biplate :: [String])
          ==> ["foo","bar","!"]

      it "ints" $
        ((("foo", "bar"), "!", 2 :: Int, ())
           ^.. biplate :: [Int])
          ==> [2]

      it "chars" $
        ((("foo", "bar"), "!", 2 :: Int, ())
           & biplate %~ toUpper)
          ==> (("FOO","BAR"),"!",2,())

      it "head" $
        ((("foo","bar"),"!", 2 :: Int, ())
           & (biplate
                :: Data s
                => Traversal' s String)._head
           %~ toUpper)
          ==> (("Foo","Bar"),"!",2,())

      it "partsOf" $
        (("foo","bar"),"!", 2 :: Int, ())
          & partsOf biplate
          %~ (reverse :: String -> String)
          ==> (("!ra","boo"),"f",2,())

      it "filtered" $
        (("foo","bar"),"!", 2 :: Int, ())
          & partsOf (biplate.filtered (<= 'm'))
          %~ (reverse :: String -> String)
          ==> (("!oo","abr"),"f",2,())

-- Local Variables:
-- haskell-indent-spaces: 2
-- haskell-indentation-ifte-offset: 2
-- haskell-indentation-layout-offset: 2
-- haskell-indentation-left-offset: 2
-- haskell-indentation-starter-offset: 2
-- haskell-indentation-where-post-offset: 2
-- haskell-indentation-where-pre-offset: 2
-- End:
