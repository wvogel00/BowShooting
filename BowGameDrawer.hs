module BowGameDrawer where 

import BowGame
import Graphics.UI.WX
import Control.Monad (foldM_)

draw :: GameState -> BitMapInfo -> DC a -> -> Rect -> IO ()
draw gameS bminfo dc rect = do
	drawLife (life gameS) dc rect
	drawScore (score gameS) (numBM bmInfo) dc rect
	drawObjs (playerObj:enemObjs) dc rect
	if bowObj /= Nothing
		then drawBow (power gameS) (fromJust $ bowObj) dc rect
		else return ()
	where
		playerObj = (pos gameS,manBM bminfo)
		bowObj = canDraw $ (bow gameS,bowBM bmInfo)
		enemObjs = map (,enemyBM bmInfo) $ enemies gameS

--残りライフを描画
drawLife n dc rect = return ()
drawScore sc bm dc rect = return ()
drawObjs objs dc rect = return ()
drawBow pw (bow,bm) dc rect = return ()


canDraw (Nothing,_) = Nothing
canDraw (Just p,bm) = Just (p,bm)
