module BowGame where

import Data.List ((\\))
import Data.Maybe

data Game a = Game a | End
data Vec a = Vec {x :: a , y :: a} deriving (Eq,Ord)
data Command =
      Jump
	| Shoot
	| Stand
	| Walk Int
	| ReStart
	| Drawing Int Int
	| Release
	 deriving Eq

type Pos = Vec
type Enemy = (Pos Int,Vec Int)

data GameState = GameState {
	score :: Int,
	power :: Vec Int,
	life :: Int,
	pos :: Pos Int,
	bow :: Maybe (Pos Int),
	enemies :: [Enemy]
} deriving (Eq,Ord)

instance Show GameState where
	show s = "POINT :" ++ show (score s)++ "\n LIFE :" ++ show (life s)

add :: (Num a) => Pos a -> Vec a -> Pos a
add p v = Vec (x p + x v) (y p + y v)

distance :: Pos Int -> Pos Int -> Float --(Integral a , Integral b) => Pos a -> Pos a -> b
distance p1 p2 = sqrt.fromIntegral $ (x p1 - x p2)^2 + (y p1 - y p2)^2

aliveEnemies :: GameState -> (Int,[Enemy])
aliveEnemies s = if bow s == Nothing || destroyed == []
	then (0,enemies s) else (length destroyed,enemies s\\destroyed)
	where
		reachPt = add (fromJust $ bow s) $ power s
		inHitArea (p,_) = distance reachPt p < 10
		destroyed = filter inHitArea $ enemies s

hitOnPlayer :: GameState -> Bool
hitOnPlayer s = (/=[]).filter inHitArea $ enemies s where
	inHitArea (p,_) = distance p (pos s) < 10

updateGame :: Game GameState -> Command -> Game GameState
updateGame End _ = End
updateGame (Game s) cm = if life s < 0
	then End
	else Game GameState {
		score = score s + point*50,
		power = power',
		life = if hitOnPlayer s then life s -1 else life s,
		pos = nextPos (pos s)  cm,
		bow = bow',
		enemies = alives
	} where
		(point,alives) = aliveEnemies s
		(bow',power') = nextBowState s cm

nextPos :: Pos Int -> Command -> Pos Int
nextPos p (Walk x) = add p (Vec x $ y p)

--矢の位置と飛力を渡す
nextBowState :: GameState -> Command -> (Maybe (Pos Int) , Vec Int)
nextBowState s Release = (Just (pos s) , power s)
nextBowState s (Drawing x y) = (bow s , add (power s) $ Vec x y )
nextBowState s _ = if 0 < fst (aliveEnemies s)  --矢が敵にヒット
	then (Nothing,Vec 0 0)
	else (Just $ add (power s) $ fromJust (bow s) , attenuate $ power s)

--矢の推進力の減衰関数
attenuate :: Vec Int -> Vec Int
attenuate (Vec x y) = Vec x $ min 0 (abs $ y - 10)
