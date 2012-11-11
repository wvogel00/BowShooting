module BowGameDrawer where 

import BowGame
import Graphics.UI.WX
import Control.Monad (foldM_)
import Graphics.UI.WX.Draw
import Data.Maybe (fromJust)

draw :: GameState -> DC a -> Rect -> IO ()
draw gameS dc rec = do
	drawRect dc (rect (pt 0 0) (sz 600 45)) [brushColor:=lightgrey,brushKind:=BrushSolid]
	drawLife (life gameS) dc rec
	drawScore (score gameS) dc rec
	drawPlayer dc (pos gameS) 0 0
	drawPower 0 True dc rec
	mapM_ (drawEnemies dc) $ enemies gameS
	if bowObj /= Nothing
		then drawPower (power gameS) (Nothing /= bowObj) dc rec
		else return ()
	where
		bowObj = canDraw $ power gameS

--残りライフを描画
drawLife n dc _ = do 
	drawText dc "LIFE" (pt 350 10) [color:=black,font:= fontFixed{_fontSize=18} ]
	foldM_ circles dc $ take n poses where
		circles dc pos = circle dc pos 15 [brushColor:=rgb 255 123 123] >> return dc
		poses = map (flip point 20) [450,475..]

--得点を描画
drawScore sc dc _ = do
	drawText dc (show sc) (pt 520 5) [color := rgb 50 120 70,font:=fontFixed{_fontSize=20,_fontShape=ShapeSlant}]

drawEnemies :: DC a -> Enemy ->IO()
drawEnemies dc (pos,_,r,c) = circle dc pos r [color:=c]

--弓の聴力、矢の射出方向、主人公の座標をとり描画する
drawPlayer :: DC a -> Pos Int -> Int -> Double -> IO()
drawPlayer dc pos tension deg = do
	archery dc pos tension deg >> player pos dc tension deg

player pos dc tension deg = do
	circle dc pos 12 [color:=black]
	line dc neck hip [color:=black]
	line dc hip (add hip $ pt (-10) 24) [color:=black]
	line dc hip (add hip $ pt 10 24) [color:=black]
	line dc neck elbow [color:=black]
	line dc elbow hand [color:=black] where
		neck = add pos $ pt 0 12
		hip = add neck $ pt 0 30
		elbow = (\p->point (pointX neck + pointX p `div` 2) (pointY neck+6)) (sub hand neck)
		hand = add pos $ point 50 0

archery :: DC a -> Pos Int -> Int -> Double -> IO()
archery dc pos tension deg = do
	arc dc pos 50 (48+deg) (-48+deg) [color:=black,penWidth:=2]
	line dc topPos pos [color:=black]
	line dc bottomPos pos [color:=black] where
		topAngle = pi/2+2*pi*(48+deg)/360
		bottomAngle = pi/2+2*pi*(-48+deg)/360
		calcPos p a = add p $ pt (floor $ 50*sin a) (floor $50*cos a)
		topPos = calcPos pos topAngle
		bottomPos = calcPos pos bottomAngle

--弓屋のパワーゲージを表示
drawPower _ False _ _ = return ()
drawPower pw True dc rec = do
	foldM_ drawRects (dc,0) $ take 10 powerMeter where
		drawRects (dc,n) m = do
			drawRect dc m [ brushColor:=switchColor n,brushKind:=BrushSolid]
			return (dc,n+1)
		switchColor n
			| n >= 7 = red
			| n >= 4 = magenta
			| otherwise = yellow
		powerMeter :: [Rect]
		powerMeter = map (flip rect (sz 12 20)) (map (flip pt 10) [20,38..])

canDraw Nothing  = Nothing
canDraw (Just p) = Just p

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

