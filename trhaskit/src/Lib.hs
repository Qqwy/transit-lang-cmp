module Lib
    ( someFunc,
    StopTime(..),
    Trip(..)
    ) where

import Control.Applicative
import GHC.Generics (Generic)
import Data.Csv (ToNamedRecord, FromNamedRecord)

import Data.ByteString (ByteString)
import Data.ByteString qualified

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data StopTime = StopTime {
        trip_id :: !ByteString,
        stop_id :: !ByteString,
        arrival_time :: !ByteString,
        departure_time :: !ByteString
    }
    deriving (Generic, Show)

instance FromNamedRecord StopTime
instance ToNamedRecord StopTime

data Trip = Trip {
        route_id :: !ByteString,
        service_id :: !ByteString,
        trip_id :: !ByteString
    }
    deriving (Generic, Show)

instance FromNamedRecord Trip
instance ToNamedRecord Trip

-- instance FromNamedRecord Trip where
--     parseNamedRecord r = 
--         pure Trip
--         <*> r .: "route_id" 
--         <*> r .: "service_id" 
--         <*> r .: "trip_id"