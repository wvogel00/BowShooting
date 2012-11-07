module BowGame where

import Data.List ((\\),delete)
import Data.Maybe
import Graphics.UI.WX (Bitmap,Point2(..),point)
import Control.Applicative

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

type Bow = (Pos Int,Vec Int)
type Pos = Point2
type Vec = Point2

type Enemy = (Pos Int,Vec Int)

data GameState = GameState {
	score :: Int,
	power :: Maybe (Vec Int),
	life :: Int,
	pos :: Pos Int,
	bows :: [Bow],
	enemies :: [Enemy]
} deriving (Eq,Ord)

instance Show GameState where
	show s = "POINT :" ++ show (score s)++ "\n LIFE :" ++ show (life s)

initGameState = GameState{
	score = 0,
	power = Nothing,
	life = 3,
	pos = point 50 240,
	bows = [],
	enemies = []
	}

add :: (Num a) => Pos a -> Vec a -> Pos a
add p v = point (pointX p + pointX v) (pointY p + pointY v)

distance :: Pos Int -> Pos Int -> Float --(Integral a , Integral b) => Pos a -> Pos a -> b
distance p1 p2 = sqrt.fromIntegral $ (pointX p1 - pointX p2)^2 + (pointY p1 - pointY p2)^2

aliveEnemies :: GameState -> (Int,[Enemy])
aliveEnemies s = if bows s == [] || destroyed == []
	then (0,enemies s) else (length destroyed,enemies s\\destroyed)
	where
		reachPoses = map (\(p,v) -> (add p $point 5 5,v)) $ bows s
		inHitArea (p1,_) p2 = distance p1 p2 < 10
		destroyed = filter alives $ enemies s
		alives enem = any (inHitArea enem) $ map fst reachPoses

hitOnPlayer :: GameState -> Bool
hitOnPlayer s = (/=[]).filter inHitArea $ enemies s where
	inHitArea (p,_) = distance p (pos s) < 10

updateGame :: Game GameState -> Command -> Game GameState
updateGame End _ = End
updateGame (Game s) cm = if life s < 0
	then End
	else Game GameState {
		score = score s + point*50,
		power = power s,
		life = if hitOnPlayer s then life s -1 else life s,
		pos = nextPos (pos s)  cm,
		bows = mkNextBows s cm $ bows s,
		enemies = alives
	} where
		(point,alives) = aliveEnemies s

nextPos :: Pos Int -> Command -> Pos Int
nextPos p (Walk x) = add p (point x $ pointY p)

--矢の位置とその矢の飛力を返す
mkNextBows :: GameState -> Command -> [Bow] -> [Bow]
mkNextBows s Release bs = (pos s,fromJust $ power s):bs
mkNextBows s _ bs = if 0 < fst (aliveEnemies s)  --矢が敵にヒット
	then foldl (flip delete) bs $ snd $ aliveEnemies s
	else bs

--矢の推進力の減衰関数
attenuate :: Vec Int -> Vec Int
attenuate p = point (pointX p) $ min 0 (abs $ pointY p - 10)
