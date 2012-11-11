import Graphics.UI.WX
import BowGame
import BowGameDrawer
import Data.IORef
import Control.Applicative
import Control.Monad (liftM)
import DataParser hiding (color)
import ProcessEvent

main = start bowGUI

bowGUI = do
	state <- newIORef (Start (pt 160 100) (pt 5 5))
	f <- frame [text := "shooting",clientSize:=sz width height]
	p <- panel f [on paint := onPaint state]
	t <- timer f [interval := 20 ,on command := nextState state p]

	enem <- getData =<< readFile "MapData.en"
	case enem of
		Nothing -> print "file reading error" >> close f
		Just es -> do
			set p [ on click := clickEvent es state f,
					on mouse := dealPower state,
					on enterKey := clickEvent' es state f,
					on rightKey := moveObject state 5,
					on leftKey  := moveObject state (-5),
					on (charKey 'z') := writeIORef state opening]
			--set f [layout := minsize (sz width height) $ widget p]

onPaint :: IORef (Game GameState) -> DC() -> Rect -> IO()
onPaint sIO dc area = do
	s <- readIORef sIO
	case s of
		Start sp _ -> drawOpening sp dc area
		Game state -> do
			draw state dc area
		End c -> drawEnd c dc area

nextState :: IORef (Game GameState) -> Panel() -> IO()
nextState sIO p = modifyIORef sIO (flip update Release) >> repaint p
