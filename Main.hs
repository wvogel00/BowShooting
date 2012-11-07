import Graphics.UI.WX
import Graphics.UI.WXCore (bitmapGetSize)
import BowGame
import BowGameDrawer

main = start bowGUI

bowGUI :: IO()
bowGUI = do
	f <- frame [text := "shooting", on paint := onPaint (point 10 10), clientSize := sz 600 320]
	repaint f

onPaint :: Point2 Int -> DC() -> Rect -> IO()
onPaint pt dc area = do
	drawArchery (point 140 220) 0 dc area
	draw initGameState dc area
