module BowGame where

import System.Random
import Data.List ((\\),delete)
import Data.Maybe
import Graphics.UI.WX (Bitmap,Point2(..),point,Color(..))
import Control.Applicative

width = 600 :: Int
height = 320 :: Int

data Game a = Start (Point2 Int) (Vec Int) | Game a | End Choise
data Choise = Continue | Quit deriving Eq
data Command = Shoot | Stand | Drawing Int Int | Release deriving Eq

type Bow = (Pos Int,Vec Int)
type Pos = Point2
type Vec = Point2

type Enemy = (Pos Int,Vec Int,Int,Color)

data GameState = GameState {
	score :: Int,
	power :: Maybe (Vec Int),
	life :: Int,
	pos :: Pos Int,
	bows :: [Bow],
	enemies :: [Enemy]
} deriving Eq

instance Show GameState where
	show s = "POINT :" ++ show (score s)++ "\n LIFE :" ++ show (life s)

opening = Start (point 160 100) (point 5 5)

initializeGame es = GameState{
	score = 0,
	power = Nothing,
	life = 3,
	pos = point 50 240,
	bows = [],
	enemies = es
	}
rand :: Int -> Double
rand = fst.randomR (0,1.0).mkStdGen

add :: (Num a) => Pos a -> Vec a -> Pos a
add p v = point (pointX p + pointX v) (pointY p + pointY v)

sub :: (Num a) => Pos a -> Vec a -> Pos a
sub p1 p2 = point (pointX p2 - pointX p1) (pointY p2 - pointY p1)

distance :: Pos Int -> Pos Int -> Float --(Integral a , Integral b) => Pos a -> Pos a -> b
distance p1 p2 = sqrt.fromIntegral $ (pointX p1 - pointX p2)^2 + (pointY p1 - pointY p2)^2

aliveEnemies :: GameState -> (Int,[Enemy])
aliveEnemies s = if bows s == [] || destroyed == []
	then (0,enemies s) else (length destroyed,enemies s\\destroyed)
	where
		reachPoses = map (\(p,v) -> (add p $point 5 5,v)) $ bows s
		inHitArea (p1,_,_,_) p2 = distance p1 p2 < 10
		destroyed = filter alives $ enemies s
		alives enem = any (inHitArea enem) $ map fst reachPoses

hitOnPlayer :: GameState -> Bool
hitOnPlayer s = (/=[]).filter inHitArea $ enemies s where
	inHitArea (p,_,r,_) = distance p (pos s) < fromIntegral r

--今の座標と進行方向から進行方向を決定する
reflect :: Point2 Int -> Vec Int -> Vec Int
reflect p v = point x' y' where
	inWidth = notOverWin 0 width $ pointX p + pointX v
	inHeight = notOverWin 50 height $ pointY p + pointY v
	x' = if inWidth then pointX v else -pointX v
	y' = if inHeight then pointY v else -pointY v

notOverWin :: Int -> Int -> Int -> Bool
notOverWin edge1 edge2 p = p < edge2 && edge1 < p

--ゲーム状態の更新を行う
update :: Game GameState -> Command -> Game GameState
--Opening
update (Start pos vec) _ =
	let pos' = add pos vec
	in Start pos' $ reflect (add (point 100 30) pos') vec
--Ending
update (End lr) _ = End lr
--Game
update (Game s) cm = if life s < 0
	then End Continue
	else Game GameState {
		score = score s + point*50,
		power = power s,
		life = if hitOnPlayer s then life s -1 else life s,
		pos = pos s,
		bows = mkNextBows s cm $ bows s,
		enemies = map moveEnemy alives
	} where
		(point,alives) = aliveEnemies s
		canMove p v = inArea $ add p v
		inArea p = pointX p>= 0 && pointX p< width && pointY p>= 0 && pointY p<height
		enemyPos p v = if canMove p v then add p v else p
		moveEnemy (p,v,r,c) = (enemyPos p v , reflect p v , r , c)

--矢の位置とその矢の飛力を返す
mkNextBows :: GameState -> Command -> [Bow] -> [Bow]
mkNextBows s Release bs = (pos s,fromJust $ power s):bs
mkNextBows s _ bs = filter (inArea.fst) bs where
	inArea p = notOverWin 0 width (pointX p)&&notOverWin 50 height (pointY p)

--矢の推進力の減衰関数
attenuate :: Vec Int -> Vec Int
attenuate p = point (pointX p) $ min 0 (abs $ pointY p - 10)
