module Game.Minecraft.Region where

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
import Game.Minecraft.Chunk

-- | The (X,Z) coordinates specifying a 'Region'
type RegionCoords = (Int, Int)

-- | A region contains a collection of 'Chunk's
-- TODO: Replace bytestring with actual chunk data
data Region = Region (Vector (Maybe Chunk))
            deriving Show

-- | Don't care about timestamps yet
getTimestamps :: Get ()
getTimestamps = replicateM_ 1024 (get :: Get Word32)

getRawRegion :: Get (Vector Location, L.ByteString)
getRawRegion = do
  locV <- getLocations
  getTimestamps
  chunkData <- getLazyByteString . fromIntegral =<< remaining
  return (locV, chunkData)

instance Serialize Region where
  get = do raw <- getRawRegion
           return $ Region (getChunks raw)
  put = undefined

-- | Given 'ChunkCoords', gives back the 'RegionCoords' containing
-- that chunk
chunkToRegionCoords :: ChunkCoords -> RegionCoords
chunkToRegionCoords (x, z) = (x `shift` (-5), z `shift` (-5))

regionToChunk :: S.ByteString -> Chunk
regionToChunk regionData = 
    let (Right (Region v)) = decode regionData
        (Just (Chunk c))   = (V.!) v 1023
        (Right nbt)        = decodeLazy c
    in Chunk nbt

readRegion :: FilePath -> RegionCoords -> IO (Either String Region)
readRegion world coords = decode <$> S.readFile (world </> "region" </> regionFileName coords) 

readAvailableRegionCoords :: FilePath -> IO [RegionCoords]
readAvailableRegionCoords world = do 
    allFiles <- getDirectoryContents $ world </> "region"
    let regionFiles = filter isRegionFile allFiles
    return $ fmap fileToCoords regionFiles
    where
        isRegionFile = (== ".mcr") . takeExtension
        fileToCoords regionFile =                          -- r.x.z.mcr
            let fileName       = dropExtension regionFile  -- r.x.z 
                (fileName', z) = splitExtension fileName   -- r.x       .z
                x              = takeExtension fileName'   --           .x
            in ((read . tail) x, (read . tail) z)          -- (x, z)

-- | Given 'RegionCoords', gives back the filename of the region file
-- containing that region
regionFileName :: RegionCoords -> FilePath
regionFileName (x, z) = "r" <.> show x <.> show z <.> "mcr"

testRegion = decode <$> S.readFile ("testWorld/region" </> regionFileName (-1,-1)) :: IO (Either String Region)

testChunk = do (Right (Region v)) <- testRegion
               let (Just (Chunk c)) = (V.!) v 1023
                   (Right nbt) = decodeLazy c
               return (nbt :: NBT)

testBlocks = do (CompoundTag _ [(CompoundTag (Just "Level") ts)]) <- testChunk
                return $ filter (\t -> case t of (ByteArrayTag (Just "Blocks") _ _) -> True; _ -> False) ts

testBlockIds :: IO [BlockId]
testBlockIds = do [(ByteArrayTag _ _ bs)] <- testBlocks
                  return (map (toEnum . fromIntegral) (S.unpack bs))

testBlockColumns :: IO [[[BlockId]]]
testBlockColumns = chunksOf 16 <$> chunksOf 128 <$> testBlockIds 

testCrossSection :: Int -> IO [[BlockId]]
testCrossSection level = (fmap . fmap . fmap) (!! level) testBlockColumns
