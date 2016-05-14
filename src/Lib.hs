module Lib
    ( paletteThem
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
                    in best : bestPerm xs l

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
