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
import Data.Text qualified

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data StopTime = StopTime {
        trip_id :: !Text,
        stop_id :: !Text,
        arrival_time :: !Text,
        departure_time :: !Text
    }
    deriving (Generic, Show)

instance FromNamedRecord StopTime
instance ToNamedRecord StopTime
instance ToJSON StopTime where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON StopTime

data Trip = Trip {
        route_id :: !Text,
        service_id :: !Text,
        trip_id :: !Text
    }
    deriving (Generic, Show)

instance FromNamedRecord Trip
instance ToNamedRecord Trip
instance ToJSON Trip where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Trip

data ScheduleResponse = ScheduleResponse {
        trip_id :: !Text,
        service_id :: !Text,
        route_id :: !Text,
        schedules :: !(Vector StopTime)
    }
    deriving (Generic, Show)

instance ToJSON ScheduleResponse where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ScheduleResponse