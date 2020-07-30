module SmDedupe
  ( getChartkeys
  , notDot
  )
where

import qualified Data.ByteString.Lazy.Char8    as ByteString
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Digest.Pure.SHA
import           Data.Ratio
import           SmDedupe.Parse
import           Control.Monad
import           Control.Arrow

-- | Insert ending beat into parsed bpm values
withEndBeats :: [(b, c)] -> b -> [(b, b, c)]
withEndBeats songBpms totalBeats =
  let (startBeats, values) = unzip songBpms
      endBeats             = tail startBeats ++ [totalBeats]
  in  zip3 startBeats endBeats values


-- | Add duration in minutes of given beat to acc
foldBeats :: Num a => a -> (a, a, a) -> a
foldBeats acc (startBeat, endBeat, value) = acc + (endBeat - startBeat) * value

-- | Get the average bpm of a song when it has multiple bpms
averageBpm' :: Song -> Double
averageBpm' (Song songBpms (firstChart : _)) =
  let totalBeats = fromIntegral $ length firstChart * 4
      beatsSum   = foldl foldBeats 0 (withEndBeats songBpms totalBeats)
  in  beatsSum / totalBeats

-- | Get the average bpm of a song, checking if the song only has one bpm
averageBpm :: Song -> Double
averageBpm (Song songBpms songCharts) = if length songBpms == 1
  then snd $ head songBpms
  else averageBpm' (Song songBpms songCharts)

-- | Check if the value of a row is "0000"
notEmpty :: (String, b) -> Bool
notEmpty = ("0000" /=) . fst

-- | Get total beats of a bpm section
beatDuration :: Fractional a => (a, a, a) -> a
beatDuration (startBeat, endBeat, value) = (endBeat - startBeat) / value

-- | Get the moment that a row occurs in minutes
rowMoment
  :: (Fractional a1, Integral a2, Ord a1)
  => [(a1, a1, a1)]
  -> a2
  -> a2
  -> a2
  -> a1
rowMoment plusEndBeats measureLen previousMeasures currentMeasureBeat =
  let currentBeat =
          4
            * fromIntegral (previousMeasures * measureLen + currentMeasureBeat)
            / fromIntegral measureLen
      previousBpms          = filter ((<= currentBeat) . fst3) plusEndBeats
      (startBeat, _, value) = last previousBpms
  in  sum (map beatDuration $ init previousBpms)
        + (currentBeat - startBeat)
        / value

mapMeasure
  :: (Num b1, Enum b1) => (Int -> b2 -> b1 -> c) -> ([a], b2) -> [(a, c)]
mapMeasure rowMoment (measure, previousMeasures) =
  let rowMoment' = rowMoment (length measure) previousMeasures
  in  zipWith (curry (second rowMoment')) measure [0 ..]

-- | Get the moment that each row occurs in minutes
insertMoments
  :: (Fractional b, Ord b) => [(b, b)] -> [[String]] -> [(String, b)]
insertMoments songBpms measures = filter notEmpty $ concat $ zipWith
  (curry
    ( mapMeasure
    $ rowMoment
    $ withEndBeats songBpms
    $ fromIntegral
    $ length measures
    * 4
    )
  )
  measures
  [0 ..]

-- | Check if the current row happens before the next snap, if it does, assign the current
-- row either the current snap or the next snap, depending on which one is closer. Continue
-- until the row list is empty and return the accumulated values.
quantize'
  :: (Ord b, Fractional b)
  => [(a, Rational)]
  -> Ratio Integer
  -> Integer
  -> b
  -> [(a, b)]
  -> [(a, Rational)]
quantize' quant _ __ ___ [] = reverse quant
quantize' quant currentSnap duration avgBpm ((row, timing) : chart) =
  let
    proportion = timing / (fromIntegral duration / avgBpm)
    next       = currentSnap + (1 % (duration * 192))
  in
    if proportion < fromRational next
      then
        if (fromRational next - proportion)
           > (proportion - fromRational currentSnap)
        then
          quantize' ((row, currentSnap) : quant) next duration avgBpm chart
        else
          quantize' ((row, next) : quant) next duration avgBpm chart
      else quantize' quant next duration avgBpm ((row, timing) : chart)

-- | Quantize the given chart to the closest 192nd snap of the average bpm of the song
quantize :: RealFrac a1 => a1 -> [(a2, a1)] -> [(a2, Rational)]
quantize avgBpm chart =
  quantize' [] (0 % 1) (ceiling $ maximum (map snd chart) * avgBpm) avgBpm chart

-- | Quantize the charts in a song
quantized :: Song -> [[(String, Rational)]]
quantized song =
  map (quantize (averageBpm song) . insertMoments (bpms song)) (charts song)

-- | Get the chartkey of a single chart
getChartkey :: Show a => [(String, a)] -> Digest SHA1State
getChartkey ((firstRow, firstMoment) : rows) =
  sha1
    $  ByteString.pack
    $  firstRow
    ++ ","
    ++ show firstMoment
    -- TODO: make more readable
    ++ concatMap (uncurry ((("," ++) .) . (. (("," ++) . show)) . (++))) rows

-- | Get all the chartkeys for a song
getChartkeys :: Song -> Set (Digest SHA1State)
getChartkeys = Set.fromList . map getChartkey . quantized

-- | Return True if a path is "." or ".."
notDot :: String -> Bool
notDot = liftM2 (&&) ("." /=) (".." /=)

-- | Get the first element of a triplet
fst3 :: (a, b, c) -> a
fst3 (value, _, __) = value
