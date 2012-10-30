import Graphics.UI.WX
import Graphics.UI.WXCore (bitmapGetSize)
import BowGame

main = start bowGUI

bowGUI :: IO()
bowGUI = do
	bmInfo <- initBitMap
	size <- bitmapGetSize (manBM bmInfo)
	f <- frame [text := "弓撃ち", on paint := onPaint (point 10 10,manBM bmInfo), clientSize := sz 600 320]
	repaint f

onPaint :: (Point2 Int,Bitmap()) -> DC() -> Rect -> IO()
onPaint (pt,bm) dc area = drawBitmap dc bm pt False []

initBitMap :: IO BitMapInfo
initBitMap = do
	man <- bitmapCreateFromFile "img/stand.png"
	bow <- bitmapCreateFromFile "img/stand.png"
	enemy <- bitmapCreateFromFile "img/stand.png"
	nums <- bitmapCreateFromFile "img/stand.png"
	return $ BitMapInfo{
		manBM = man, bowBM = bow, enemyBM = enemy,numBM = nums
		}
