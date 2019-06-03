module Debug.Trace.Dump
    ( traceFile
    , traceFileBSL
    , traceFileText
    , traceBSLSuffix
    , traceTextSuffix
    ) where

import           Control.Applicative  (pure)
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor         (($>), (<$>))
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Word            (Word)
import           Numeric              (showHex)
import           System.Directory     (doesFileExist)
import           System.IO.Unsafe     (unsafePerformIO)
import           System.Random        (randomIO)

randHex :: IO String
randHex = ($"") . (showHex :: Word -> ShowS) <$> randomIO

-- | This is a utility function that enables dumping data from multiple
-- function calls without overwriting anything.
traceBSLSuffix :: String -- ^ File extension, e.g. @.json@
               -> BSL.ByteString
               -> a
               -> a
traceBSLSuffix ext bytes x = unsafePerformIO $ do
    fpBase <- randHex
    let fp = fpBase ++ ext
    check <- doesFileExist fp
    if check
        then pure $ traceBSLSuffix ext bytes x
        else BSL.writeFile fp bytes $> x

traceTextSuffix :: String -- ^ File extension, e.g. @.json@
                -> T.Text
                -> a
                -> a
traceTextSuffix ext txt x = unsafePerformIO $ do
    fpBase <- randHex
    let fp = fpBase ++ ext
    check <- doesFileExist fp
    if check
        then pure $ traceTextSuffix ext txt x
        else TIO.writeFile fp txt $> x

traceFileBSL :: FilePath -> BSL.ByteString -> a -> a
traceFileBSL fp bytes x = unsafePerformIO $
    BSL.writeFile fp bytes $> x

traceFileText :: FilePath -> T.Text -> a -> a
traceFileText fp txt x = unsafePerformIO $
    TIO.writeFile fp txt $> x

traceFile :: FilePath -> String -> a -> a
traceFile fp str x = unsafePerformIO $
    writeFile fp str $> x
