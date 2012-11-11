import Graphics.UI.WX
import BowGame
import BowGameDrawer
import Data.IORef
import Control.Applicative
import Control.Monad (liftM)
import DataParser hiding (color)

main = start bowGUI

bowGUI :: IO()
bowGUI = do
	state <- newIORef (Start (pt 160 100) (pt 5 5))
	filedata <- readFile "MapData.en"
	f <- frame [text := "shooting",clientSize:=sz width height]
	p <- panel f [on paint := onPaint state]
	t <- timer f [interval := 60 ,on command := updateGameState state p]

	enem <- getData filedata
	case enem of
		Nothing -> print "file reading error" >> close f
		Just es -> do
			set p [ on click := clickEvent es state f,
					on mouse := dealPower state,
					on enterKey := clickEvent' es state f,
					on rightKey := moveObject state 5,
					on leftKey  := moveObject state (-5)]
			--set f [layout := minsize (sz width height) $ widget p]

dealPower :: IORef (Game GameState) -> EventMouse -> IO()
dealPower sIO mState = do
	--let pos = mousePos mState
	s <- readIORef sIO
	case s of
		End _ -> return()
		Start _ _ -> return ()
		Game state -> case mState of
			MouseLeftUp pos _-> if power state == Nothing
				then return ()
				else modifyIORef sIO (\(Game st) ->(Game $ st {power = flip sub pos<$>power st}))
			MouseLeftDown pos _ -> return ()
			_ -> return ()

--IORefで渡しているので、draw関数を修正する必要、またはonPaint関数を削除する可能性あり
onPaint :: IORef (Game GameState) -> DC() -> Rect -> IO()
onPaint sIO dc area = do
	s <- readIORef sIO
	case s of
		Start sp _ -> drawOpening sp dc area
		Game state -> do
			drawArchery (point 140 220) 0 dc area
			draw state dc area
		End c -> drawEnd c dc area

moveObject :: IORef (Game GameState) -> Int -> IO()
moveObject sIO k = do
	s <- readIORef sIO
	case s of
		Start _ _ -> return()
		Game _ -> movePlayer sIO k
		End Continue -> writeIORef sIO (End Quit)
		End Quit -> writeIORef sIO (End Continue)

--主人公の位置を更新
movePlayer :: IORef (Game GameState) -> Int -> IO()
movePlayer sIO k = do
	s <- readIORef sIO
	modifyIORef sIO (move k) where
		pos' s = add (pos s) $ pt k 0
		inArea = notOverWin width.pointX.pos'
		move k (Game st) = if inArea st then Game $ st {pos = pos' st} else Game st

clickEvent' :: [Enemy] -> IORef (Game GameState) -> Frame () -> IO()
clickEvent' es sIO f = do
	s <- readIORef sIO
	case s of
		Game _ -> return ()
		_ -> clickEvent es sIO f (pt 0 0)

--左マウスクリックイベント（矢を番える、start,end画面の更新）
clickEvent :: [Enemy] -> IORef (Game GameState) -> Frame ()-> Point -> IO()
clickEvent es sIO f pt = do
	s <- readIORef sIO
	case s of
		Start _ _ -> writeIORef sIO (Game $ initializeGame es)
		End Continue -> writeIORef sIO (Start (point 160 100) (point 5 5))
		End Quit -> close f
		Game _ -> modifyIORef sIO initPower where
			initPower (Game st) = Game $ st {power = Just pt}

--オープニングを描画
drawOpening :: Point2 Int -> DC() -> Rect -> IO()
drawOpening pos dc area = do
	dcClear dc
	drawRect dc (rect (pt 0 0) (sz width height)) [brushColor:=rgb 160 45 12]
	drawText dc "弓撃ちゲーム" pos [brushColor:=green,font:=fontFixed{_fontSize=25}]

--ゲームオーバー画面描画
drawEnd :: Choise -> DC() -> Rect -> IO()
drawEnd lr dc area = do
	let pointer = [pt 60 248,pt 60 272,pt 90 260,pt 60 248]
	dcClear dc
	drawRect dc (rect (pt 0 0) (sz width height)) [brushColor:=rgb 20 20 40]
	drawText dc "ゲームオーバー" (pt 160 100) [color:=red,brushColor:=red,font:=fontFixed{_fontSize=25}]
	drawText dc "続ける" (pt 100 240) [color:=white,font:=fontFixed{_fontSize=20}]
	drawText dc "やめる" (pt 400 240) [color:=white,font:=fontFixed{_fontSize=20}]

	case lr of
		Continue -> polygon dc pointer [brushColor:=yellow]
		Quit -> polygon dc (map (add (point 290 0)) pointer) [brushColor:=yellow]

updateGameState :: IORef (Game GameState) -> Panel() -> IO()
updateGameState sIO p = do
	modifyIORef sIO (flip update Release)
	repaint p
