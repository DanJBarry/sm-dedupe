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

-- | Insert ending beat into parsed bpm values
withEndBeats :: [(b, c)] -> b -> [(b, b, c)]
withEndBeats songBpms totalBeats =
  let (startBeats, values) = unzip songBpms
  in  let endBeats = (tail startBeats ++ [totalBeats])
      in  zip3 startBeats endBeats values

-- | Add duration in minutes of given beat to acc
foldBeats :: Num a => a -> (a, a, a) -> a
foldBeats acc (startBeat, endBeat, value) = acc + (endBeat - startBeat) * value

-- | Get the average bpm of a song
averageBpm :: Song -> Float
averageBpm (Song songBpms songCharts) = if (length songBpms) == 1
  then snd $ head songBpms
  else
    let totalBeats = fromIntegral $ (length $ head $ songCharts) * 4
    in  let plusEndBeats = withEndBeats songBpms totalBeats
        in  let beatsSum = foldl foldBeats 0 plusEndBeats
            in  beatsSum / totalBeats

-- | Check if the value of a row is "0000"
notEmpty :: (String, b) -> Bool
notEmpty (row, _) = row /= "0000"

-- | Get total beats of a bpm section
beatDuration :: Fractional a => (a, a, a) -> a
beatDuration (startBeat, endBeat, value) = (endBeat - startBeat) / value

-- | Get the moment that each row occurs in minutes
insertMoments
  :: (Foldable t1, Fractional b, Foldable t2, Ord b)
  => [(b, b)]
  -> t1 (t2 String)
  -> [(String, b)]
insertMoments songBpms measures =
  let totalBeats = toInteger $ (length measures) * 4
  in
    let plusEndBeats = withEndBeats songBpms $ fromInteger totalBeats
    in
      filter notEmpty $ concat $ foldl
        (\acc measure ->
          acc
            ++ [ foldl
                   (\acc' row ->
                     acc'
                       ++ [ ( row
                            , let
                                currentBeat =
                                  4
                                    * ( ( fromIntegral (length acc)
                                        * fromIntegral (length measure)
                                        )
                                      + fromIntegral (length acc')
                                      )
                                    / fromIntegral (length measure)
                              in
                                let previousBpms = filter
                                      (\(startBeat, _, __) ->
                                        startBeat <= currentBeat
                                      )
                                      plusEndBeats
                                in  (sum $ map beatDuration $ init previousBpms)
                                      + let (startBeat, _, value) =
                                              last previousBpms
                                        in  (currentBeat - startBeat) / value
                            )
                          ]
                   )
                   []
                   measure
               ]
        )
        []
        measures

-- | Check if the current row happens before the next snap, if it does, assign the current
-- row either the current snap or the next snap, depending on which one is closer. Continue
-- until the row list is empty and return the accumulated values.
quantize'
  :: (Ord b, Fractional b)
  => [(a, b)]
  -> [(a, Rational)]
  -> Rational
  -> Integer
  -> b
  -> [(a, Rational)]
quantize' [] quant _ __ ___ = quant
quantize' ((row, timing) : chart) quant currentSnap duration avgBpm =
  let proportion = timing / (fromIntegral duration / avgBpm)
  in
    let next = currentSnap + (1 % (duration * 192))
    in
      if proportion < fromRational next
        then
          if (fromRational next - proportion)
             > (proportion - fromRational currentSnap)
          then
            quantize' chart
                      (quant ++ [(row, currentSnap)])
                      currentSnap
                      duration
                      avgBpm
          else
            quantize' chart (quant ++ [(row, next)]) currentSnap duration avgBpm
        else quantize' ((row, timing) : chart) quant next duration avgBpm

-- | Quantize the given chart to the closest 192nd snap of the average bpm of the song
quantize :: RealFrac a1 => [(a2, a1)] -> a1 -> [(a2, Rational)]
quantize chart avgBpm =
  let duration = ceiling $ (maximum $ map snd chart) * avgBpm
  in  quantize' chart [] (0 % 1) duration avgBpm

-- | Quantize the charts in a song
quantized :: Song -> [[(String, Rational)]]
quantized song =
  let avgBpm = averageBpm song
  in  map (\chart -> quantize (insertMoments (bpms song) chart) avgBpm)
        $ charts song

-- | Append a quantized row to a string
foldQuantized :: Show a => String -> (String, a) -> String
foldQuantized acc (row, moment) = acc ++ "," ++ row ++ "," ++ show moment

-- | Get the chartkey of a single chart
getChartkey :: Show a => [(String, a)] -> Digest SHA1State
getChartkey ((firstRow, firstMoment) : rows) = sha1 $ ByteString.pack $ foldl
  foldQuantized
  (firstRow ++ "," ++ show firstMoment)
  rows

-- | Get all the chartkeys for a song
getChartkeys :: Song -> Set (Digest SHA1State)
getChartkeys song = Set.fromList $ map getChartkey $ quantized song

-- | Return True if a path is "." or ".."
notDot :: String -> Bool
notDot path = path /= "." && path /= ".."
