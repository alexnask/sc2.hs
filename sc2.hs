{-# LANGUAGE OverloadedStrings #-}

module Sc2 where

import MPQ
import Data.ByteString
import Data.Bits
import Prelude hiding (take, drop, head)
import Data.ByteString.Char8 hiding (head, take, drop)
import Data.Word
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Clock.POSIX

data Player = Player { name     :: ByteString
                      , race     :: ByteString
                      , team     :: Int
                      , handicap :: Int
                      , win      :: Bool
                      , color    :: (Int, Int, Int, Int) -- rgba
                      }
             | Observer { name   :: ByteString } deriving (Show)

data Replay = Replay { players :: [Player]
                      , region  :: ByteString
                      , map     :: ByteString
                      , time    :: ByteString
                      , account :: ByteString
                      } deriving (Show)

data BlizzStruct = ArrayData [BlizzStruct] | StringData ByteString | HashMapData [(Int, BlizzStruct)] | IntData Int | Unknown
                  deriving(Show)

word :: [Word8] -> Word32
word (a:b:c:d:[]) = (fromIntegral a `shiftL` 24)
            .|. (fromIntegral b `shiftL` 16)
            .|. (fromIntegral c `shiftL`  8)
            .|. (fromIntegral d            )

readDataStruct :: ByteString -> BlizzStruct
readDataStruct dat = fst $ readDataStruct' dat 0

-- Well, this is a giant mess :p
-- See https://github.com/GraylinKim/sc2reader/blob/master/sc2reader/utils.py => read_data_struct

readDataStruct' :: ByteString -> Int -> (BlizzStruct, Int)
readDataStruct' dat offset =
    if flag == 0x02 then
        let (length, off) = variableInt dat (offset + 1) in
        (StringData $ take length (drop off dat), off + length)
    else if flag == 0x04 then
        if ((head $ take 1 (drop (offset + 1) dat)) == 1) && ((head $ take 1 (drop (offset + 2) dat)) == 0) then
            let (nelements, off) = variableInt dat (offset + 3)
                (elements, noff) = getN dat nelements off in
                (ArrayData elements, noff)
        else (Unknown, offset + 1)
    else if flag == 0x05 then
        let (nentries, off) = variableInt dat (offset + 1)
            (entries, noff) = getNkeys dat nentries off in
        (HashMapData entries, noff)
    else if flag == 0x06 then
        (IntData $ fromIntegral $ head $ take 1 (drop (offset + 1) dat), offset + 2)
    else if flag == 0x07 then
        (IntData $ fromIntegral $ word $ Data.ByteString.unpack $ take 4 (drop (offset + 1) dat), offset + 5)
    else if flag == 0x09 then
        let (num, off) = variableInt dat (offset + 1) in
        (IntData num, off)
    else (Unknown, offset)

    where flag = head $ take 1 (drop offset dat)
          getN :: ByteString -> Int -> Int -> ([BlizzStruct], Int)
          getN _ 0 off = ([], off)
          getN dat n off = let (firstStruct, firstOff) = readDataStruct' dat off
                               (restStructs, restOff) = getN dat (n - 1) firstOff in
                           (firstStruct : restStructs, restOff)

          getNkeys :: ByteString -> Int -> Int -> ([(Int, BlizzStruct)], Int)
          getNkeys _ 0 off = ([], off)
          getNkeys dat n off = let (key, offset) = variableInt dat off
                                   (firstStruct, firstOff) = readDataStruct' dat offset
                                   (restStructs, restOff) = getNkeys dat (n - 1) firstOff in
                               ((key, firstStruct) : restStructs, restOff)

          variableInt :: ByteString -> Int -> (Int, Int)
          variableInt dat off = variableInt' dat (off + 1) (fromIntegral $ head $ take 1 (drop off dat)) 0 0
                              where variableInt' :: ByteString -> Int -> Int -> Int -> Int -> (Int, Int)
                                    variableInt' dat off byte val i = if (byte .&. 0x80) > 0 then
                                                                        let value = val + ((byte .&. 0x7F) `shiftL` (7 * i)) in
                                                                        variableInt' dat (off + 1) (fromIntegral $ head $ take 1 (drop off dat)) value (i + 1)
                                                                    else
                                                                        let value = val + (byte `shiftL` (7 * i)) in
                                                                        (((- 1) ^ (value .&. 0x1)) * (value `shiftR` 1), off)

loadReplay :: ByteString -> IO Replay
loadReplay path = do
                      let Left archive = openArchive path
                          -- Parse replay.initData: See https://github.com/GraylinKim/sc2reader/wiki/replay.initData for info on the magic happening here
                          Just numInit = fileNumber archive "replay.initData"
                          Just init = fileContents archive (fromIntegral numInit) 
                          playerNum = head $ take 1 init
                          (names, offset) = getNames init playerNum 1
                          -- I had a little bit of trouble here. The docs say there are 24 bytes of unknown data, I found out that in patch 1.5 replays there are actually 32 bytes
                          accountLength = head $ take 1 (drop (offset + 33) init)
                          account = take (fromIntegral accountLength) (drop (offset + 34) init)
                           -- Also, the docs indicate we have 684 bytes of padding after the account string while I found out there are 686 bytes
                          realm = getRealm init (offset + 33 + (fromIntegral accountLength) + 686)
                          -- Finished parsing replay.initData
                          -- Lets parse replay.details!
                          Just numDetails = fileNumber archive "replay.details"
                          Just details = fileContents archive (fromIntegral numDetails)
                          struct = readDataStruct details
                          (players, map, date) = getInfo struct
                      print names
                      print account
                      print realm
                      print players
                      print map
                      print date
                      return $ Replay [] realm map (Data.ByteString.Char8.pack date) account
                      where
                          getNames _ 0 off = ([], off)
                          getNames dat num off = let len = head $ take 1 (drop off dat)
                                                     (next, noff) = getNames dat (num - 1) (off + (fromIntegral len) + 6) in
                                                 (take (fromIntegral len) (drop (off + 1) dat) : next, noff)
                          getRealm dat off = if take 4 (drop off dat) == "s2ma" then
                                                 take 2 (drop (off + 6) dat)
                                             else
                                                 "Unknown"
                          getInfo (HashMapData pairs) = let ArrayData playerArray = snd $ pairs !! 0
                                                            StringData map = snd $ pairs !! 1
                                                            IntData timestamp = snd $ pairs !! 5
                                                            (year, month, day) = toGregorian $ utctDay $ posixSecondsToUTCTime $ realToFrac (((realToFrac timestamp) - 116444735995904000)/10000000) in
                                                            (playerArray, map, (show day) ++ "/" ++ (show month) ++ "/" ++ (show year))


{- Test code -}
test :: String -> IO ()
test file = do
                rep <- loadReplay $ Data.ByteString.Char8.pack file
                return ()

