module Text.DecimalOrFraction where

import Control.Applicative
import Data.Char (digitToInt)
import Data.Ratio ((%))
import Text.Trifecta

-- Choosing Double for now
type DecimalOrFraction = Either Double Rational

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimalComponent :: Parser Double
parseDecimalComponent =
  let go acc base = (digit >>= \d -> go ((acc + ((fromInteger . toInteger . digitToInt) d) / base)) (base * 10)) <|> return acc
  in go (fromInteger 0) 10

parseDecimal :: Parser Double
parseDecimal = do
  whole <- fromInteger <$> decimal
  char '.'
  fractional <- parseDecimalComponent
  return $ (whole / 1) + fractional

parseDecimalOrFraction :: Parser DecimalOrFraction
parseDecimalOrFraction =
  (Left <$> parseDecimal) <|> (Right <$> parseFraction)
