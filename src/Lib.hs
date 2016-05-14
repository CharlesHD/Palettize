module Lib
    ( paletteThem'
    ) where
import Codec.Picture
import System.Directory
import Control.Monad
import Data.List
import qualified Data.ByteString.Lazy as B
import System.Info

dirSep :: String
dirSep = if os == "linux" then "/" else "\\"

isImage :: FilePath -> Bool
isImage p = any (`isSuffixOf` p) formats
        where formats = [".jpg", ".jpeg", ".png", ".gif"]

paletteThem :: FilePath -> FilePath ->  IO ()
paletteThem paletteFile imgDir =
            do dynimg <- readImage paletteFile
               let palette = fmap convertRGB8 dynimg
               files <- getDirectoryContents imgDir
               let imgs = filter isImage files
               either putStrLn (\x -> mapM_ (paletteIt imgDir x) imgs) palette

concatImage :: Pixel a => Image a -> Image a -> Image a
concatImage a b = generateImage fc (imageWidth a + imageWidth b) (imageHeight a)
            where fc x y = if x < imageWidth a then pixelAt a x y else pixelAt b (x - imageWidth a) y

concatImages :: [Image PixelRGB8] -> Image PixelRGB8
concatImages [] = generateImage (\_ _ -> PixelRGB8 0 0 0) 0 0
concatImages (x:xs) = foldl' concatImage x xs

splitImages :: Pixel a => Image a -> Int -> [Image a]
splitImages a n = if (imageWidth a < n) then []
		  else generateImage (\x y -> pixelAt a x y) n (imageHeight a) : splitImages (generateImage (\x y -> pixelAt a (x + n) y) (imageWidth a - n) (imageHeight a)) n

paletteThem' :: FilePath -> FilePath -> IO ()
paletteThem' pFile imgDir =
             do dynimg <- readImage pFile
	        let palette = fmap convertRGB8 dynimg
		let palOpt = fmap mkPalOpt palette
		files <- getDirectoryContents imgDir
		let imgFiles = filter isImage files
		imgs <- mapM readImage imgFiles
		let imgs' = mapM (fmap convertRGB8) imgs
		let width = fmap (imageWidth . head) imgs'
		let bigImg = fmap concatImages imgs'
		let paletted = liftM2 palettize palOpt palette
		let pal2 = fmap snd paletted
		let palImg = fmap fst paletted
		let newPal = liftM2 reorderPalette palette pal2
		let pImgs = liftM2 splitImages palImg width
		let act = fmap sequence_ $ (\(p, xs) -> mapM (\(i, f) -> writeGifImageWithPalette (imgDir ++ dirSep ++ "res" ++ dirSep ++ f ++ ".gif") i p) (zip xs imgFiles)) =<< liftM2 (,) newPal pImgs
		either putStrLn id act

mkPalOpt :: Palette -> PaletteOptions
mkPalOpt p = PaletteOptions {
         paletteCreationMethod = MedianMeanCut,
         enableImageDithering = True,
         paletteColorCount = imageWidth p
}

reorderPalette :: Palette -> Palette -> Palette
reorderPalette p p' = let pl  = [pixelAt p  x 0 | x <- [0.. imageWidth p - 1]]
                          pl' = [pixelAt p' x 0 | x <- [0.. imageWidth p - 1]]
                          perm =  bestPerm pl' pl
                          in generateImage (\x _ -> perm !! x) (imageWidth p) (imageHeight p)

bestPerm :: [PixelRGB8] -> [PixelRGB8] -> [PixelRGB8]
bestPerm _ [] = []
bestPerm [] _ = []
bestPerm (x:xs) l = let best = minimumBy (comparePerm x) l
                        comparePerm x p1 p2 = compare (distance x p1) (distance x p2)
                        l' = filter (/= best) l
                    in best : bestPerm xs l'

distance :: PixelRGB8 -> PixelRGB8 -> Int
distance (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = abs $ (fromIntegral r2 - fromIntegral r1) * (fromIntegral g2 - fromIntegral g1) * (fromIntegral b2 - fromIntegral b1)

paletteIt :: FilePath -> Palette -> FilePath -> IO ()
paletteIt dir pal file = do dynimg <- readImage $ dir ++ dirSep ++ file
                            putStrLn $ "treating : " ++ file
                            createDirectoryIfMissing True (dir ++ dirSep ++ "res")
                            let img = paletteItAnnexe pal =<< dynimg
                            either putStrLn (writePng (dir ++ dirSep ++ "res" ++ dirSep ++ file ++ ".png")) img
                            putStrLn "done"


paletteItAnnexe :: Palette -> DynamicImage -> Either String (Image PixelRGBA8)
paletteItAnnexe p img =
    let img' = convertRGBA8 img
        appendAlpha a =
            generateImage (\x y -> let (PixelRGB8 r g b) = pixelAt a x y
                                       (PixelRGBA8 _ _ _ alp) = pixelAt img' x y
                                   in PixelRGBA8 r g b alp) (imageWidth a) (imageHeight a)
        palOpt = mkPalOpt p
        (pImg, p2) = palettize palOpt
                               (pixelMap (\(PixelRGBA8 r g b _ ) -> PixelRGB8 r g b) img')
        p3 = reorderPalette p p2
    in do gifByte <- encodeGifImageWithPalette pImg p3
          gif <- decodeImage $ B.toStrict gifByte
          let rgb = convertRGB8 gif
          return $ appendAlpha rgb
