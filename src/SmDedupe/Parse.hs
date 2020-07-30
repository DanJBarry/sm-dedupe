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

data Section = Bpms [(Double, Double)] | Chart [[String]] | Extra

data Song = Song
  { bpms :: [(Double, Double)],
    charts :: [[[String]]]
  }
  deriving (Show)

endSection :: Parser ()
endSection = do
  char ';'
  manyTill anyChar $ skipMany1 (char '#') <|> eof
  return ()

lastMeasure :: Parser [String]
lastMeasure = do
  measure <- manyTill row $ char ';'
  spaces'
  return measure

row :: Parser String
row = do
  result <- manyTill (noneOf "/") spaces'
  sepEndBy comment spaces'
  return result


rows :: Parser [String]
rows = sepEndBy1 (many1 alphaNum) endOfLine

comma :: Parser ()
comma = do
  spaces'
  char ','
  spaces'

measures :: Parser [[String]]
measures = sepBy1 rows comma

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
  -- ???
  manyTill anyChar $ char ':'
  spaces'
  (measures : _) <- manyTill measures endSection
  return $ Chart measures

extra :: Parser Section
extra = do
  manyTill anyChar endSection
  spaces'
  return Extra

bpm :: Parser (Double, Double)
bpm = do
  beat  <- manyTill anyChar $ char '='
  value <- many $ noneOf ",;"
  return (read beat, read value)

singleBpm :: Parser [(Double, Double)]
singleBpm = do
  beat  <- manyTill anyChar $ char '='
  value <- manyTill (noneOf ",") endSection
  return [(read beat, read value)]

multipleBpms :: Parser [(Double, Double)]
multipleBpms = do
  (result : _) <- manyTill (sepBy1 bpm comma) endSection
  return result

parseBpms :: Parser Section
parseBpms = do
  result <- try singleBpm <|> multipleBpms
  return $ Bpms result

section :: Parser Section
section = do
  title <- manyTill anyChar $ string ":"
  case title of
    "NOTES" -> notes
    "BPMS"  -> parseBpms
    _       -> extra

comment :: Parser ()
comment = do
  string "//"
  manyTill anyChar endOfLine
  return ()

parseSm :: Parser Song
parseSm = do
  manyTill anyChar $ char '#'
  results <- many section
  return $ foldl
    (\song section -> case section of
      Bpms  newBpms  -> song { bpms = newBpms }
      Chart newChart -> song { charts = newChart : charts song }
      Extra          -> song
    )
    Song { bpms = [], charts = [] }
    results

-- thank you for putting zero-width spaces in your .sm files
spaceOrIgnorable :: Parser Char
spaceOrIgnorable = space <|> satisfy ((Format ==) . generalCategory)

spaces' :: Parser ()
spaces' = do
  skipMany spaceOrIgnorable
  result <- sepEndBy comment $ skipMany spaceOrIgnorable
  return ()
