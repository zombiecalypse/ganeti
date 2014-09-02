{-| @/proc/stat@ data collector.

-}

{-

Copyright (C) 2013 Google Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

-}

module Ganeti.DataCollectors.CPUload
  ( dcName
  , dcVersion
  , dcFormatVersion
  , dcCategory
  , dcKind
  , dcReport
  , dcUpdate
  ) where

import qualified Control.Exception as E
import Data.Attoparsec.Text.Lazy as A
import Data.Text.Lazy (pack, unpack)
import qualified Text.JSON as J
import qualified Data.Sequence as Seq
import System.Posix.Unistd (getSysVar, SysVar(ClockTick))

import qualified Ganeti.BasicTypes as BT
import qualified Ganeti.Constants as C
import Ganeti.Cpu.LoadParser(cpustatParser)
import Ganeti.DataCollectors.Types
import Ganeti.Utils
import Ganeti.Cpu.Types

-- | The default path of the CPU status file.
-- It is hardcoded because it is not likely to change.
defaultFile :: FilePath
defaultFile = C.statFile

-- | The buffer size of the values kept in the map.
bufferSize :: Int
bufferSize = C.cpuavgloadBufferSize

-- | The window size of the values that will export the average load.
windowSize :: Integer
windowSize = toInteger C.cpuavgloadWindowSize

-- | The default setting for the maximum amount of not parsed character to
-- print in case of error.
-- It is set to use most of the screen estate on a standard 80x25 terminal.
-- TODO: add the possibility to set this with a command line parameter.
defaultCharNum :: Int
defaultCharNum = 80*20

-- | The name of this data collector.
dcName :: String
dcName = C.dataCollectorCPULoad

-- | The version of this data collector.
dcVersion :: DCVersion
dcVersion = DCVerBuiltin

-- | The version number for the data format of this data collector.
dcFormatVersion :: Int
dcFormatVersion = 1

-- | The category of this data collector.
dcCategory :: Maybe DCCategory
dcCategory = Nothing

-- | The kind of this data collector.
dcKind :: DCKind
dcKind = DCKPerf

-- | The data exported by the data collector, taken from the default location.
dcReport :: Maybe CollectorData -> IO DCReport
dcReport colData =
  let extractColData (CPULoadData v) = v
      cpuLoadData = maybe Seq.empty extractColData colData
  in buildDCReport cpuLoadData

-- | Data stored by the collector in mond's memory.
type Buffer = Seq.Seq (Timestamp, [Int])

-- | Compute the load from a CPU.
computeLoad :: CPUstat -> Int
computeLoad cpuData =
  csUser cpuData + csNice cpuData + csSystem cpuData
  + csIowait cpuData + csIrq cpuData + csSoftirq cpuData
  + csSteal cpuData + csGuest cpuData + csGuestNice cpuData

-- | Reads and Computes the load for each CPU.
dcCollectFromFile :: FilePath -> IO (Timestamp, [Int])
dcCollectFromFile inputFile = do
  contents <-
    ((E.try $ readFile inputFile) :: IO (Either IOError String)) >>=
      exitIfBad "reading from file" . either (BT.Bad . show) BT.Ok
  cpustatData <-
    case A.parse cpustatParser $ pack contents of
      A.Fail unparsedText contexts errorMessage -> exitErr $
        show (Prelude.take defaultCharNum $ unpack unparsedText) ++ "\n"
          ++ show contexts ++ "\n" ++ errorMessage
      A.Done _ cpustatD -> return cpustatD
  now <- getCurrentTime
  let timestamp = now :: Integer
  return (timestamp, map computeLoad cpustatData)

-- | Returns the collected data in the appropriate type.
dcCollect :: IO Buffer
dcCollect  = do
  l <- dcCollectFromFile defaultFile
  return (Seq.singleton l)

-- | Formats data for JSON transformation.
formatData :: [Double] -> CPUavgload
formatData [] = CPUavgload (0 :: Int) [] (0 :: Double)
formatData l@(x:xs) = CPUavgload (length l - 1) xs x

-- | Update a Map Entry.
updateEntry :: Buffer -> Buffer -> Buffer
updateEntry newBuffer mapEntry =
  (Seq.><) newBuffer
  (if Seq.length mapEntry < bufferSize
    then mapEntry
    else Seq.drop 1 mapEntry)

-- | Updates the given Collector data.
dcUpdate :: Maybe CollectorData -> IO CollectorData
dcUpdate mcd = do
  v <- dcCollect
  let new_v =
        case mcd of
          Nothing -> v
          Just cd ->
            case cd of
              CPULoadData old_v -> updateEntry v old_v
  new_v `seq` return $ CPULoadData new_v

-- | Computes the average load for every CPU and the overall from data read
-- from the map. Returns Bad if there are not enough values to compute it.
computeAverage :: Buffer -> Integer -> Integer -> DataResponse [Double]
computeAverage s w ticks = go (Seq.viewl window) (Seq.viewr window)
  where
    err = BT.Bad . ErrorMessage
    window = Seq.takeWhileL ((> w) . fst) s
    dividedBy x = flip (/) (fromIntegral x) . fromIntegral
    go Seq.EmptyL          _                    = err "Empty buffer"
    go _                   Seq.EmptyR           = err "Empty buffer"
    go (leftmost Seq.:< _) (_ Seq.:> rightmost) = do
      let (timestampL, listL) = leftmost
          (timestampR, listR) = rightmost
          workInWindow = zipWith (-) listL listR
          overall = (timestampL - timestampR) * ticks
      if overall > 0
        then BT.Ok $ map (dividedBy overall) workInWindow
        else err $ "Time covered by data is not sufficient."
                 ++ "The window considered is " ++ show w

-- | This function computes the JSON representation of the CPU load.
buildJsonReport :: Buffer -> IO J.JSValue
buildJsonReport v = do
  ticks <- getSysVar ClockTick
  let res = computeAverage v windowSize ticks
  return $ BT.genericResult J.showJSON (J.showJSON . formatData) res

-- | This function computes the DCReport for the CPU load.
buildDCReport :: Buffer -> IO DCReport
buildDCReport v  =
  buildJsonReport v >>=
    buildReport dcName dcVersion dcFormatVersion dcCategory dcKind
