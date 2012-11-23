module Game.Minecraft.Chunk where
import Codec.Compression.Zlib
import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Serialize
import qualified Data.Serialize.Builder as Builder
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Data.List.Split
import System.FilePath
import System.Directory

import Data.NBT
import Game.Minecraft.Block
import Game.Minecraft.Location

-- | The (X,Z) coordinates specifying a 'Chunk'
type ChunkCoords = (Int, Int)

data Chunk = Chunk L.ByteString

instance Show Chunk where 
  show _ = "<chunk>"

getChunks :: (Vector Location, L.ByteString) -> Vector (Maybe Chunk)
getChunks (locV, chunkData) = V.map getChunk locV
  where
    getChunk (Loc 0 0)                = mzero
    getChunk (Loc offset sectorCount) = 
      return . Chunk . either error id . runGetLazy extractChunk $
             L.take (4096 * (fromIntegral sectorCount))
                    (L.drop (4096 * (fromIntegral (offset - 2))) chunkData)
    extractChunk = do 
      len <- fromIntegral <$> getWord32be
      compScheme <- getWord8
      case compScheme of
        1 -> fail "GZip-compressed chunks not supported"
        2 -> decompress . L.fromChunks . (:[]) <$> ensure (len-1)


-- | Converts a chunk into a list of block ids contained
chunkToBlocks :: NBT -> [BlockId]
chunkToBlocks (CompoundTag _ [(CompoundTag (Just "Level") ts)]) = 
    let [(ByteArrayTag _ _ bs)] = filter (\t -> case t of (ByteArrayTag (Just "Blocks") _ _) -> True; _ -> False) ts
    in map (toEnum . fromIntegral) (S.unpack bs)

chunkToBlockColumns :: NBT -> [[[BlockId]]]
chunkToBlockColumns = chunksOf 16 . chunksOf 128 . chunkToBlocks

horzCrossSection :: Int -> [[[BlockId]]] -> [[BlockId]]
horzCrossSection = fmap . fmap . flip (!!) 
