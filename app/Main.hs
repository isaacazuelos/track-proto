{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Prelude hiding (init, log)

import Options.Generic

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Yaml as Yaml

import Data.Time.Format.Human (humanReadableTime)
import System.FilePath.Posix ((</>), (<.>))
import System.Directory (getCurrentDirectory, doesFileExist)

data Options
  = Init
  | Status
  | Log
  | Start 
  | Stop  { message :: Maybe String <?> "Leave a log message" }
  deriving (Show, Generic)

instance ParseRecord Options

data Status
  = Started { at :: Time.UTCTime }
  | Stopped  
  deriving (Show, Generic)

data Event
  = Event
    { duration   :: Time.NominalDiffTime
    , logMessage :: Maybe String
    }
  deriving (Show, Generic)

data Track
  = Track
    { current :: Status
    , events  :: [Event]
    }
  deriving (Show, Generic)

instance Yaml.FromJSON Track
instance Yaml.ToJSON Track

instance Yaml.FromJSON Status
instance Yaml.ToJSON Status

instance Yaml.FromJSON Event
instance Yaml.ToJSON Event

empty = Track Stopped []

getTrackPath = (</> ".track" <.> ".yml") <$> getCurrentDirectory

readTrack path = do
  contents <- ByteString.readFile path
  case Yaml.decodeEither contents of
    Right track -> return (track :: Track)
    Left  err   -> error err

writeTrack path track = ByteString.writeFile path (Yaml.encode track)

main = do
  record <- getRecord "track how much time you're wasting"
  case record of
    Init   -> init
    Status -> status
    Log    -> log
    Start  -> start
    Stop m -> stop  (unHelpful m)
    
init = do
  path <- getTrackPath
  exists <- doesFileExist path -- Yeah, race conditions. It doesn't matter here.
  case exists of
    True  -> putStrLn ".track file already exists"
    False -> ByteString.writeFile path (Yaml.encode empty) >> putStrLn "initialized"

status = do
  path <- getTrackPath
  Track status events <- readTrack path
  putStr $ "project "
  case status of
    Stopped   -> putStrLn "is stopped"
    Started start -> do
      time <- humanReadableTime start
      putStrLn $ "was started " ++ time
  putStrLn $ show (length events) ++ " event(s)"

start = do
  path <- getTrackPath
  Track status events <- readTrack path
  case status of
    Started _ -> putStrLn "project is already started"
    Stopped   -> do
      now <- Time.getCurrentTime
      writeTrack path $ Track (Started now) events
      putStrLn "started"

stop msg = do
  path <- getTrackPath
  Track status events <- readTrack path
  case status of
    Stopped   -> putStrLn "project is already stopped"
    Started start -> do
      now <- Time.getCurrentTime
      let dur = Time.diffUTCTime now start
      writeTrack path $ Track Stopped ((Event dur msg):events)
      putStrLn "stopped"

log =
  do
    Track s events <- getTrackPath >>= readTrack
    status
    mapM_ (putStrLn . prettyEvent) events
  where
    prettyEvent (Event dur Nothing) = "worked for " ++ (show dur)
    prettyEvent (Event dur (Just m)) = "worked for " ++ (show dur) ++ ": " ++ m
      
