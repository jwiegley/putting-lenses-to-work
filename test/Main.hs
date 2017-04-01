{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Lens
import Data.Char
import Data.Text (Text)
-- import qualified Data.Text as T
import Test.Hspec

default (Int, Text)

data Record = Record
    { _field1 :: Int
    , _field2 :: Int
    }
    deriving (Eq, Show)

makeLenses ''Record

data ADT = Alpha Int Int
         | Beta Record
         | Gamma Text
    deriving (Eq, Show)

makePrisms ''ADT

digit :: Int -> Traversal' Int Int
digit n f x = fmap (read . concat) $
    sequence [show x] & ix n %%~ fmap show . f . read

digit' :: Int -> Traversal' Int Int
digit' n f x = fmap read $
    show x & ix n %%~ fmap intToDigit . f . digitToInt

infixr 0 ==>
(==>) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(==>) = shouldBe

main :: IO ()
main = hspec $ do
    describe "lenses" $
        describe "products" $ do
            describe "view" $ do
                it "function" $
                    view _2 (1,2,3)
                        ==> 2

                it "operator" $
                    (1,2,3) ^. _2
                        ==> 2

            it "set" $
                (1,2,3) & _2 .~ 20
                    ==> (1,20,3)

    describe "prisms" $
        describe "ADTs" $ do
            it "preview" $
                Alpha 10 20 ^? _Alpha._2
                    ==> Just 20

            it "setter" $
                Alpha 10 20 & _Alpha._2 .~ 2
                    ==> Alpha 10 2

            it "review" $
                _Alpha # (30,40)
                    ==> Alpha 30 40

    describe "traversals" $ do
        describe "collections" $ do
            it "getter" $
                [1,2,3] ^? ix 1
                    ==> Just 2

            it "setter" $
                [1,2,3] & ix 1 .~ 20
                    ==> [1,20,3]

        describe "computations" $ do
            it "getter" $
                31415926 ^? digit 2
                    ==> Just 4

            it "setter" $
                31415926 & digit 2 .~ 8
                    ==> 31815926

        describe "records" $ do
            it "getter" $
                let r = Record 20 30 in
                r ^. field1 ==> 20

            it "setter" $
                let r = Record 20 30 in
                (r & field1 .~ 1) ==> Record 1 30

    --     describe "Maps" $ do

    --     describe "ADTs" $ do
    --     describe "Maps" $ do

-- >>> [1,2,3,4] & mapped +~ 1
-- [2,3,4,5]

-- >>> let x = "{\"prefixes\": [{\"prefix\": \"blah\"}, {\"red\": \"blue\"}]}"
-- >>> x ^.. key "prefixes".values.filtered (has (key "red".only "blue"))
-- [Object (fromList [("red",String "blue")])]

-- foo (view _2 -> Foo x) = print ("a foo! " ++ show x)

-- >>> has (ix 4) [1]
-- False

-- >>> has _Left (Left 5)
-- True

-- >>> [] ^?! (ix 0 `failing` like 42)
-- 42

-- >>> ("Hello","Heinrich") & both %~ length
-- (5, 8)

-- >>> ("abcdef","ghijkl") ^. both
-- "abcdefghijkl"

-- >>> ("abcdef","ghijkl") % partsOf (both.traverse) .~ "hi there"
-- ("hi the","reijkl")

-- >>> ("hello","world") % partsOf (both.traverse) %~ reverse
-- ("dlrow","olleh")

-- >>> ["a","bc","def"] % partsOf (traverse.traverse) %~ reverse
-- ["f","ed","cba"]

-- >>> "Hello, World" % partsOf (traverse . filtered isAlpha) .~ "Howdy!!!"
-- "Howdy, !orld"

-- >>> (`evalStateT` ("hello","world")) $ zoom both (get >>= lift . print)
-- "hello"
-- "world"

-- >>> partsOf both %~ reverse $ ('a','b')
-- ('b','a')

-- >>> ((1,2), (3,4)) ^. alongside _2 _1
-- (2,3)

-- >>> [(1,2,3,4),(5,6,7,8),(9,10,11,12)]^.folded._4.to Sum
-- Sum {getSum = 24}

-- >>> ("hello","there")^.._1.replicated 3.traverse.to toUpper
-- "HELLOHELLOHELLO"

-- >>> (("hello","world"),"!!!", 2, ()) ^..biplate :: [String]
-- ["hello","world","!!!"]

-- >>> (("hello","world"),"!!!", 2, ()) ^..biplate :: [Int]
-- [2]

-- >>> (("hello","world"),"!!!", 2, ()) & biplate %~ toUpper
-- (("HELLO","WORLD"),"!!!",2,())

-- >>> (("hello","world"),"!!!", 2, ()) & biplate._head %~ toUpper
-- (("Hello","World"),"!!!",2,())

-- λ> ("Hello", 10, 20, "World", ["!!!"]) & partsOf biplate %~ (reverse :: String -> String)
-- ("!!!dl",10,20,"roWol",["leH"])

-- >>> minimumOf both (1,2)
-- Just 1

-- >>> rezip $ zipper ("hello","world") & down _1 & fromWithin traverse & focus .~ 'J' & farthest Control.Lens.right & focus .~ 'y'
-- ("Jelly","world")

-- >>> sequenceAOf _3 (1,2,"hello")
-- [(1,2,'h'),(1,2,'e'),(1,2,'l'),(1,2,'l'),(1,2,'o')]

-- >>> singleton 4 & contains 2 .~ True
-- fromList [2,4]

-- λ> (1,2,3) ^.. each
-- [1,2,3]

-- >>> (1,2,3) & partsOf each %~ reverse
-- (3,2,1)

-- >>> "HelloWorld" & partsOf (traverse.filtered isLower) %~ reverse
-- "HdlroWolle"

-- >>> "HelloWorld" & partsOf (both.traverse.filtered isLower) %~ reverse
-- ("Hdlro", "Wolle")

-- >>> ("Hello","World",["Neat","!!!"]) & partsOf (biplate.filtered isLower) %~ (L.reverse :: String -> String)
-- ("Htaed","Wlroo",["Nlle","!!!"])

-- >>> "68656c6c6f20776f726c64"^..chunking 2 folded.base 16.to chr
-- "hello world"

-- >>> "00:00:00" & upon (tail.tail).partsOf (biplate.filtered (== '0')) .~ "1234"
-- "00:12:34"

-- >>> [Left (0,1),Right (1,2)] & traverse.failing (_Left._2) (_Right._2) .~ 5
-- [Left (0,5),Right (1,5)]


-- Getter 1 r
-- Setter 0+ w
-- Lens 1 rw
-- Iso entire rw
-- Traversal 0+ rw
-- Fold 0+ r
