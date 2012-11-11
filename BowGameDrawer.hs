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
	drawPlayer dc (pos gameS)
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

--主人公と敵を描画
drawObjs (p:es) dc _ = do
	drawPlayer dc p

drawEnemies :: DC a -> Enemy ->IO()
drawEnemies dc (pos,vec,r,c) = do
	circle dc (sub pos $ pt width 0) r [color:=c]

drawPlayer dc pos = do
	circle dc pos 12 [color:=black]
	line dc neck hip [color:=black]
	line dc hip (add hip $ pt (-10) 24) [color:=black]
	line dc hip (add hip $ pt 10 24) [color:=black] where
		neck = add pos $ pt 0 12
		hip = add neck $ pt 0 30

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

--弓の描画
drawArchery :: Point -> Double -> DC () -> Rect -> IO()
drawArchery pos deg dc rect = do
	arc dc pos 50 (48+deg) (-48+deg) [color:=blue]
	line dc topPos pos [color:=green]
	line dc bottomPos pos [color:=cyan] where
		(topAngle,bottomAngle) = (pi/2+2*pi*(48+deg)/360,pi/2+2*pi*(-48+deg)/360)
		topPos = add pos $ pt (floor $ 50*sin topAngle) (floor $50*cos topAngle)
		bottomPos = add pos $ pt (floor $ 50*sin bottomAngle) (floor $ 50*cos bottomAngle)

canDraw Nothing  = Nothing
canDraw (Just p) = Just p
