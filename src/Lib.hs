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

groupThem :: [a] -> [[a]]
groupThem [] = []
groupThem [x] = [[x, x]]
groupThem [x, y] = groupThem [x, y, y]
groupThem (x:y:z:xs) = [x,y] : groupThem' (x:y:z:xs)
  where groupThem' (a:b:c:ls) = [a, b, c] : groupThem' (b:c:ls)
        groupThem' [a,b] = [[a, b, b]]
        groupThem' other = groupThem other

splitMiddle :: Pixel a => Image a -> Int -> Image a
splitMiddle img w = let wt = imageWidth img
                    in if wt == 2 * w
                          then generateImage (pixelAt img) w (imageHeight img)
                          else generateImage (\x y -> pixelAt img (x + w) y) w (imageHeight img)

concatImage :: Pixel a => Image a -> Image a -> Image a
concatImage a b = generateImage fc (imageWidth a + imageWidth b) (imageHeight a)
            where fc x y = if x < imageWidth a then pixelAt a x y else pixelAt b (x - imageWidth a) y

concatImages :: [Image PixelRGBA8] -> Image PixelRGBA8
concatImages [] = generateImage (\_ _ -> PixelRGBA8 0 0 0 0) 0 0
concatImages (x:xs) = foldl' concatImage x xs

splitImages :: Pixel a => Image a -> Int -> [Image a]
splitImages a n = if imageWidth a < n then []
                  else generateImage (pixelAt a) n (imageHeight a) : splitImages (generateImage (\x y -> pixelAt a (x + n) y) (imageWidth a -                   n) (imageHeight a)) n

paletteThem' :: FilePath -> FilePath -> IO ()
paletteThem' pFile imgDir =
             do dynimg <- readImage pFile
                files <- getDirectoryContents imgDir
                let imgFiles = sort $ filter isImage files
                imgs <- mapM readImage imgFiles
                createDirectoryIfMissing True (imgDir ++ dirSep ++ "res")
                let res = do dimg <- dynimg
                             imgs' <- sequence imgs
                             let palette = convertRGB8 dimg
                             let imgs'' = map convertRGBA8 imgs'
                             let width = imageWidth $ head imgs''
                             let grpImgs = groupThem imgs''
                             let imgsWithName = zip imgFiles grpImgs
                             ios <- mapM (paletteThatGroup imgDir palette width) imgsWithName
                             return $ sequence_ ios
                either putStrLn id res

paletteThatGroup :: FilePath -> Palette -> Int -> (FilePath,[Image PixelRGBA8]) -> Either String (IO ())
paletteThatGroup imgDir pal w (f, imgs) = do let bigIm = concatImages imgs
                                             bigP <- paletteItAnnexe pal (ImageRGBA8 bigIm)
                                             let bigIm' = splitMiddle bigP w
                                             return $ do putStrLn $ "treating : " ++ f
                                                         writePng (imgDir ++ dirSep ++ "res" ++ dirSep ++ f ++ ".png") bigIm'
                                                         putStrLn "done"



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
