module BowGame where

import Data.List ((\\))
import Data.Maybe
import Graphics.UI.WX (Bitmap,Point2(..),point)

data Game a = Game a | End
data Command =
      Jump
	| Shoot
	| Stand
	| Walk Int
	| ReStart
	| Drawing Int Int
	| Release
	 deriving Eq

type Pos = Point2
type Vec = Point2

type Enemy = (Pos Int,Vec Int)

data BitMapInfo = BitMapInfo {
	manBM   :: Bitmap(),
	bowBM   :: Bitmap(),
	enemyBM :: Bitmap(),
	numBM :: Bitmap()
	}

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
add p v = point (pointX p + pointX v) (pointY p + pointY v)

distance :: Pos Int -> Pos Int -> Float --(Integral a , Integral b) => Pos a -> Pos a -> b
distance p1 p2 = sqrt.fromIntegral $ (pointX p1 - pointX p2)^2 + (pointY p1 - pointY p2)^2

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
nextPos p (Walk x) = add p (point x $ pointY p)

--矢の位置と飛力を渡す
nextBowState :: GameState -> Command -> (Maybe (Pos Int) , Vec Int)
nextBowState s Release = (Just (pos s) , power s)
nextBowState s (Drawing x y) = (bow s , add (power s) $ point x y )
nextBowState s _ = if 0 < fst (aliveEnemies s)  --矢が敵にヒット
	then (Nothing,point 0 0)
	else (Just $ add (power s) $ fromJust (bow s) , attenuate $ power s)

--矢の推進力の減衰関数
attenuate :: Vec Int -> Vec Int
attenuate p = point (pointX p) $ min 0 (abs $ pointY p - 10)
