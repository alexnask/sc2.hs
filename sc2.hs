{-# LANGUAGE OverloadedStrings #-}

module Sc2 where

import MPQ
import Data.ByteString
import Prelude hiding (take, drop, head)
import Data.ByteString.Char8 hiding (head, take, drop)

data Player = Player { name     :: ByteString
                      , race     :: ByteString
                      , team     :: Int
                      , handicap :: Int
                      , win      :: Bool
                      } deriving (Show)

data Replay = Replay { players :: [Player]
                      , region  :: ByteString
                      , map     :: ByteString
                      , time    :: ByteString
                      , account :: ByteString
                      } deriving (Show)

openReplay :: ByteString -> IO Replay
openReplay path = do
                      let Left archive = openArchive path
                          -- Parse replay.initData: See https://github.com/GraylinKim/sc2reader/wiki/replay.initData for info on the magic happening here
                          Just num = fileNumber archive "replay.initData"
                          Just dat = fileContents archive (fromIntegral num) 
                          playerNum = head $ take 1 dat
                          (names, offset) = getNames dat playerNum 1
                          -- I had a little bit of trouble here. The docs say there are 24 bytes of unknown data, I found out that in patch 1.5 replays there are actually 32 bytes
                          accountLength = head $ take 1 (drop (offset + 33) dat)
                          account = take (fromIntegral accountLength) (drop (offset + 34) dat)
                           -- Also, the docs indicate we have 684 bytes of padding after the account string while I found out there are 686 bytes
                          realm = getRealm dat (offset + 33 + (fromIntegral accountLength) + 686)
                          -- Finished parsing replay.initData
                      print names
                      print account
                      print realm
                      return $ Replay [] "" "" "" account
                      where
                          getNames _ 0 off = ([], off)
                          getNames dat num off = let len = head $ take 1 (drop off dat)
                                                     (next, noff) = getNames dat (num - 1) (off + (fromIntegral len) + 6) in
                                                 (take (fromIntegral len) (drop (off + 1) dat) : next, noff)
                          getRealm dat off = if take 4 (drop off dat) == "s2ma" then
                                                 take 2 (drop (off + 6) dat)
                                             else
                                                 "Unknown"
                  

{- Test code -}
test :: String -> IO ()
test file = do
                rep <- openReplay $ Data.ByteString.Char8.pack file
                return ()

