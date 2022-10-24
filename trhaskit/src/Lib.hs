module Lib
    ( someFunc,
    StopTime(..),
    Trip(..),
    ScheduleResponse(..)
    ) where

import Control.Applicative
import GHC.Generics (Generic)
import Data.Csv (ToNamedRecord, FromNamedRecord)
import Data.Aeson (ToJSON(..), FromJSON, genericToEncoding, defaultOptions)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text qualified

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data StopTime = StopTime {
        trip_id :: !ShortText,
        stop_id :: !ShortText,
        arrival_time :: !ShortText,
        departure_time :: !ShortText
    }
    deriving (Generic, Show)

instance FromNamedRecord StopTime
instance ToNamedRecord StopTime
instance ToJSON StopTime where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON StopTime

data Trip = Trip {
        route_id :: !ShortText,
        service_id :: !ShortText,
        trip_id :: !ShortText
    }
    deriving (Generic, Show)

instance FromNamedRecord Trip
instance ToNamedRecord Trip
instance ToJSON Trip where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Trip

data ScheduleResponse = ScheduleResponse {
        trip_id :: !ShortText,
        service_id :: !ShortText,
        route_id :: !ShortText,
        schedules :: !(Vector StopTime)
    }
    deriving (Generic, Show)

instance ToJSON ScheduleResponse where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ScheduleResponse