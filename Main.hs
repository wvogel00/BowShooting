import Graphics.UI.WX
import Graphics.UI.WXCore (bitmapGetSize)


main = start bowGUI

bowGUI :: IO()
bowGUI = do
	manBM <- bitmapCreateFromFile "img/stand.png"
	size <- bitmapGetSize manBM
	f <- frame [text := "弓撃ち", on paint := onPaint manBM, clientSize := sz 600 320]
	repaint f where
		onPaint :: Bitmap() -> DC() -> Rect -> IO()
		onPaint bm dc area = drawBitmap dc bm pointZero False []
