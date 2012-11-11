module DataParser where

import BowGame
import Text.Parsec
import Text.Parsec.String
import qualified Graphics.UI.WX as WX hiding (color)
import Control.Applicative hiding ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Char (toUpper)

getData :: String -> IO (Maybe [Enemy])
getData filedata = do
	case (parse mkData "" filedata) of
		Left err -> print err >> return Nothing
		Right xs -> return (Just xs)

mkData :: Parser [Enemy] = do
	try ( do
		pos <- WX.pt<$>num<*>(spaces *> num)
		vec <- WX.pt<$>(spaces *> num)<*>(spaces *> num)
		r <- spaces *> num <* spaces
		cl <- color.map toUpper<$>many1 letter
		spaces
		(:) (pos,vec,r,cl) <$> mkData
		)
	<|> return []

num :: Parser Int
num = read<$>many1 digit

color :: String -> WX.Color
color "RED" = WX.red
color "BLUE" =WX.blue
color "BLACK" = WX.black
color "CYAN"= WX.cyan
color "GREEN" = WX.green
