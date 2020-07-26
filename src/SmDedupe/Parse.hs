module SmDedupe.Parse
  ( Section
  , Song(Song)
  , parseSm
  , charts
  , bpms
  )
where

import           Text.Parsec
import           Text.Parsec.Text.Lazy

data Section = Bpms [(Double, Double)] | Chart [[String]] | Extra

data Song = Song
  { bpms :: [(Double, Double)],
    charts :: [[[String]]]
  }
  deriving (Show)

endSection :: Parser ()
endSection = do
  char ';'
  spaces

lastMeasure :: Parser [String]
lastMeasure = do
  measure <- manyTill row $ char ';'
  spaces
  return measure

row :: Parser String
row = manyTill anyChar spaces

rows :: Parser [String]
rows = sepEndBy1 (many1 alphaNum) endOfLine

comma :: Parser ()
comma = do
  spaces
  char ','
  spaces

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
  spaces
  (measures : _) <- manyTill measures endSection
  return $ Chart measures

extra :: Parser Section
extra = do
  manyTill anyChar endSection
  spaces
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
  begin <- string "#" <|> string "//"
  case begin of
    "#" -> do
      title <- manyTill anyChar $ string ":"
      case title of
        "NOTES" -> notes
        "BPMS"  -> parseBpms
        _       -> extra
    "//" -> comment

comment :: Parser Section
comment = do
  manyTill anyChar endOfLine
  return Extra

parseSm :: Parser Song
parseSm = do
  results <- manyTill section eof
  return $ foldl
    (\song section -> case section of
      Bpms  newBpms  -> song { bpms = newBpms }
      Chart newChart -> song { charts = charts song ++ [newChart] }
      Extra          -> song
    )
    Song { bpms = [], charts = [] }
    results
