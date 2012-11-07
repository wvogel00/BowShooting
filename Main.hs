import Graphics.UI.WX
import BowGame
import BowGameDrawer
import Data.IORef

main = start bowGUI

bowGUI :: IO()
bowGUI = do
	state <- newIORef (End Continue)
	f <- frame [text := "shooting",
				on paint := onPaint state,
				clientSize := sz 600 320,
				on click := clickEvent state,
				on rightKey := moveObject state 5,
				on leftKey := moveObject state (-5)]

	t <- timer f [interval := 25 ,on command := updateGameState state]
	repaint f

--IORefで渡しているので、draw関数を修正する必要、またはonPaint関数を削除する可能性あり
onPaint :: IORef (Game GameState) -> DC() -> Rect -> IO()
onPaint sIO dc area = do
	s <- readIORef sIO
	case s of
		Start sp -> drawOpening sp dc area
		Game state -> do
			drawArchery (point 140 220) 0 dc area
			draw initGameState dc area
		End c -> drawEnd c dc area

moveObject :: IORef (Game GameState) -> Int -> IO()
moveObject sIO k = do
	s <- readIORef sIO
	case s of
		Start _ -> return()
		Game _ -> movePlayer sIO k
		End Continue -> writeIORef sIO (End Quit)
		End Quit -> writeIORef sIO (End Continue)

--主人公の位置を更新
movePlayer :: IORef (Game GameState) -> Int -> IO()
movePlayer sIO k = do
	s <- readIORef sIO
	modifyIORef sIO (move k) where
		move k (Game st) = Game $ st {pos = add (pos st) (pt k 0)}

--左マウスクリックイベント（矢を番える、start,end画面の更新）
clickEvent :: IORef (Game GameState) -> Point -> IO()
clickEvent sIO pt = do
	s <- readIORef sIO
	case s of
		Start _ -> writeIORef sIO (Game initGameState)
		End _ -> writeIORef sIO (Start $ point 160 100)
		Game _ -> modifyIORef sIO initPower where
			initPower (Game st) = Game $ st {power = Just pt}

--オープニングを描画
drawOpening :: Point2 Int -> DC() -> Rect -> IO()
drawOpening pos dc area = do
	drawRect dc (rect (pt 0 0) (sz 600 320)) [brushColor:=rgb 160 45 12]
	drawText dc "弓撃ちゲーム" (pt 160 100) [brushColor:=green,font:=fontFixed{_fontSize=25}]

--ゲームオーバー画面描画
drawEnd :: Choise -> DC() -> Rect -> IO()
drawEnd lr dc area = do
	let pointer = [pt 60 248,pt 60 272,pt 90 260,pt 60 248]
	drawRect dc (rect (pt 0 0) (sz 600 320)) [brushColor:=rgb 20 20 40]
	drawText dc "ゲームオーバー" (pt 160 100) [color:=red,brushColor:=red,font:=fontFixed{_fontSize=25}]
	drawText dc "続ける" (pt 100 240) [color:=white,font:=fontFixed{_fontSize=20}]
	drawText dc "やめる" (pt 400 240) [color:=white,font:=fontFixed{_fontSize=20}]

	case lr of
		Continue -> polygon dc pointer [brushColor:=yellow]
		Quit -> polygon dc (map (add (point 200 0)) pointer) [brushColor:=blue]



updateGameState :: IORef (Game GameState) -> IO()
updateGameState sIO = return ()
