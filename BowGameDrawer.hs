module BowGameDrawer where 

import BowGame
import Graphics.UI.WX

data GameObject =
    Player (Pos Int)
  | Enemy (Pos Int)
  | Bow (Vec Int)
  | Arrow (Pos Int)

objects :: GameState -> [Int] -- -> [GameObject]
objects s = enemiesObj ++ [playerObj,bowObj,arrowObj]  where
	enemiesObj = map mkEnemyObj $ enemies s
	playerObj = mkPlayerObj $ pos s
	bowObj = mkBowObj $ power s -- 引き絞っている時のみ描画
	arrowObj = mkArrowObj $ bow s

mkEnemyObj :: Enemy -> Int
mkEnemyObj enem = 0

mkPlayerObj :: Pos Int -> Int
mkPlayerObj p = 1

mkArrowObj :: Maybe (Pos Int) -> Int
mkArrowObj Nothing = 0
mkArrowObj (Just p) = 1

mkBowObj :: Pos Int -> Int
mkBowObj p = 1

draw :: DC a -> [GameObject] -> IO ()
draw dc os = return ()
