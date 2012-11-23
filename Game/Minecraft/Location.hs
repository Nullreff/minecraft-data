module Game.Minecraft.Location where

import Control.Applicative
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Serialize
import qualified Data.Serialize.Builder as Builder
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word

data Location = Loc Int Word8

getWord24be :: Get Word32
getWord24be = do
  s <- getBytes 3
  return $! fromIntegral (s `S.index` 0) `shift` 16 .|.
            fromIntegral (s `S.index` 1) `shift`  8 .|.
            fromIntegral (s `S.index` 2)

putWord24be :: Putter Word32
putWord24be w | w `shift` (-24) /= 0 =
                  error "tried to put word larger than 24 bits"
              | otherwise = putBuilder $ Builder.fromByteString bytes
  where bytes = S.pack [ fromIntegral (w `shift` (-16)) :: Word8
                       , fromIntegral (w `shift`  (-8)) :: Word8
                       , fromIntegral w                 :: Word8
                       ]

instance Serialize Location where
  get = Loc . fromIntegral <$> getWord24be <*> getWord8
  put (Loc offset sectorCount) = 
    putWord24be (fromIntegral offset) >> put sectorCount

getLocations :: Get (Vector Location)
getLocations = V.replicateM 1024 (get :: Get Location)
