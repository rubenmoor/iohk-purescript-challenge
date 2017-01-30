{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Sample
  ( Track(extract)
  , Report(produce)
  , TrInput()
  , TrOutput()
  , ReOutput
  , rawData
  , trackingExample
  , reportExample)
  where

import qualified Data.Map              as M
import           Data.Text             (Text ())
import           Data.Time.Clock       (NominalDiffTime ())
import           Data.Time.Clock.POSIX (POSIXTime ())

class Track e t b a where
    extract
        :: (Foldable b, Monoid (t (b a)), Traversable t)
        => e -> t (b a) -> t (b a)

class Report r t d  where
    produce
        :: (Monoid (t d), Traversable t)
        => (t d -> r)

type TrInput = (Text, POSIXTime)
type TrOutput = (Text, NominalDiffTime)
instance Track [TrInput] [] [] TrOutput where
    extract xs ds = ds `mappend` fst (foldl f ([], undefined) xs)
      where
        f ([ys], (yu, v0)) (u,v) = ([((yu, v - v0) : ys)], (u, v))
        f _                (u,v) = ([[]],                  (u, v))

type ReInput = TrOutput
type ReOutput = M.Map Text NominalDiffTime
instance Report ReOutput [] ReInput where
    produce xs = M.fromListWith (+) xs

speakers :: [Text]
speakers = ["Jonn", "Charles", "Arseniy", "Jonn", "George", "Meeting Ended"]

timeStamps :: [Integer]
timeStamps =
    [1479081309, 1479081429, 1479081666, 1479082000, 1479083307, 1479085307]

rawData :: [TrInput]
rawData = zipWith (\x y -> (x, fromIntegral y)) speakers timeStamps

trackingExample :: [[TrOutput]]
trackingExample = extract rawData []

reportExample :: ReOutput
reportExample = (produce . (concatMap id)) trackingExample
