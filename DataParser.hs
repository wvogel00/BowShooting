module DataParser where

import BowGame
import Text.Parsec
import Text.Parsec.String
import Graphics.UI.WX hiding (color)
import Control.Applicative hiding ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Char (toUpper)

getData :: String -> IO (Maybe [Enemy])
getData filedata = do
	case (parse mkData "" filedata) of
		Left err -> print err >> return Nothing
		Right xs -> return $ Just $ map convertPos xs

convertPos (p,v,r,c) = (pt (convX p) (convY p), v, r, c)
convX p = width - pointX p
convY p = if pointY p < 50 then pointY p+50 else pointY p

mkData :: Parser [Enemy]
mkData = try ( do
	pos <- pt<$>num<*>(spaces *> num)
	vec <- pt<$>(spaces *> num)<*>(spaces *> num)
	r <- spaces *> num <* spaces
	cl <- color.map toUpper<$>many1 letter
	spaces
	(:) (pos,vec,r,cl) <$> mkData
	)
	<|> (string "end" >> return [])

num :: Parser Int
num = read<$>many1 digit

color :: String -> Color
color "RED" = red
color "BLUE" =blue
color "BLACK" = black
color "CYAN"= cyan
color "GREEN" = green
