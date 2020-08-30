module SmDedupe.Parse
  ( Section
  , Song(Song)
  , parseSm
  , charts
  , bpms
  )
where

import           Data.Char
import           Text.Parsec
import           Text.Parsec.ByteString

type BpmPair = (Double, Double)
type MeasureList = [[String]]

data Section = Bpms [BpmPair] | Chart MeasureList | Extra
data Song = Song
  { bpms :: [BpmPair],
    charts :: [MeasureList]
  }
  deriving (Show)

addSection :: Song -> Section -> Song
addSection song section = case section of
  Bpms  newBpms  -> song { bpms = newBpms }
  Chart newChart -> song { charts = newChart : charts song }
  Extra          -> song

bpm :: Parser BpmPair
bpm = do
  beat  <- manyTill anyChar $ char '='
  value <- many $ noneOf ",;"
  return (read beat, read value)

comma :: Parser ()
comma = do
  spaces'
  char ','
  spaces'

comment :: Parser ()
comment = do
  string "//"
  manyTill anyChar endOfLine
  return ()

endSection :: Parser ()
endSection = do
  char ';'
  manyTill anyChar $ skipMany1 (char '#') <|> eof
  return ()

extra :: Parser Section
extra = do
  manyTill anyChar endSection
  spaces'
  return Extra

lastMeasure :: Parser [String]
lastMeasure = do
  measure <- manyTill row $ char ';'
  spaces'
  return measure

measures :: Parser MeasureList
measures = sepBy1 rows comma

multipleBpms :: Parser [BpmPair]
multipleBpms = do
  (result : _) <- manyTill (sepBy1 bpm comma) endSection
  return result

notes :: Parser Section
notes = do
  -- type
  manyTill anyChar $ char ':'
  -- author
  manyTill anyChar $ char ':'
  -- slot
  manyTill anyChar $ char ':'
  -- difficulty
  manyTill anyChar $ char ':'
  -- radar scores
  manyTill anyChar $ char ':'
  spaces'
  (measures : _) <- manyTill measures endSection
  return $ Chart measures

parseBpms :: Parser Section
parseBpms = do
  result <- try singleBpm <|> multipleBpms
  return $ Bpms result

parseSm :: Parser Song
parseSm = do
  manyTill anyChar $ char '#'
  results <- many section
  return $ foldl addSection Song { bpms = [], charts = [] } results

row :: Parser String
row = do
  result <- manyTill (noneOf "/") spaces'
  sepEndBy comment spaces'
  return result

rows :: Parser [String]
rows = sepEndBy1 (many1 alphaNum) endOfLine

section :: Parser Section
section = do
  title <- manyTill anyChar $ string ":"
  case title of
    "NOTES" -> notes
    "BPMS"  -> parseBpms
    _       -> extra

singleBpm :: Parser [BpmPair]
singleBpm = do
  beat  <- manyTill anyChar $ char '='
  value <- manyTill (noneOf ",") endSection
  return [(read beat, read value)]

-- thank you for putting zero-width spaces in your .sm files
spaceOrIgnorable :: Parser Char
spaceOrIgnorable = space <|> satisfy ((Format ==) . generalCategory)

spaces' :: Parser ()
spaces' = do
  skipMany spaceOrIgnorable
  result <- sepEndBy comment $ skipMany spaceOrIgnorable
  return ()
