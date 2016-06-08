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

paletteThem :: FilePath -> [FilePath] ->  IO ()
paletteThem imgDir paletteFiles =
            do files <- getDirectoryContents imgDir
               let imgs = filter isImage files
               mapM_ (paletteIt imgDir paletteFiles) imgs

makeImgFromImgPalette :: Image Pixel8 -> [Image PixelRGBA8] -> Image PixelRGBA8
makeImgFromImgPalette img pixs = generateImage f w h
                      where pw = imageWidth (head pixs)
                            ph = imageHeight (head pixs)
                            iw = imageWidth img
                            ih = imageHeight img
                            w = pw * iw
                            h = ph * ih
                            f x y = let dx = div x pw
                                        dy = div y ph
                                        rx = rem x pw
                                        ry = rem y ph
                                        piximg =  pixs !! rem (fromIntegral (pixelAt img dx dy)) (length pixs)
                                    in pixelAt piximg rx ry

mkPalOpt :: [Image PixelRGBA8] -> PaletteOptions
mkPalOpt pimgs = PaletteOptions {
         paletteCreationMethod = MedianMeanCut,
         enableImageDithering = True,
         paletteColorCount = length pimgs
}

paletteIt :: FilePath -> [FilePath] -> FilePath -> IO ()
paletteIt dir pal file = do dynimg <- readImage $ dir ++ dirSep ++ file
                            p2 <- mapM readImage pal
                            putStrLn $ "treating : " ++ file
                            createDirectoryIfMissing True (dir ++ dirSep ++ "res")
                            let img = do p <- sequence p2
                                         img' <- dynimg
                                         paletteItAnnexe p img'
                            either putStrLn (writePng (dir ++ dirSep ++ "res" ++ dirSep ++ file ++ ".png")) img
                            putStrLn "done"


paletteItAnnexe :: [DynamicImage] -> DynamicImage -> Either String (Image PixelRGBA8)
paletteItAnnexe p img =
    let img' = convertRGBA8 img
        p2 = map convertRGBA8 p
        palOpt = mkPalOpt p2
        (pImg, _) = palettize palOpt
                                (pixelMap (\(PixelRGBA8 r g b _ ) -> PixelRGB8 r g b) img')
    in Right $ makeImgFromImgPalette pImg p2
