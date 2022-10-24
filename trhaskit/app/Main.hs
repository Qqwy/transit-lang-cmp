module Main (main) where

import System.Exit qualified
import Data.Csv qualified
import Data.List qualified
import Data.Vector (Vector)
import Data.Vector qualified
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified
import Data.Function (on)
import Data.Text (Text)
import Data.Text qualified
import Data.Text.Short (ShortText)
import Data.Text.Short qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.ByteString.Lazy qualified
import Data.Aeson qualified
import Web.Scotty (ActionM)
import Web.Scotty qualified
import Common ((|>))
import Lib (Trip(..), StopTime(..), ScheduleResponse(..))

type Datastore a = HashMap ShortText (Vector a)

main :: IO ()
main = do
    trips <- fetchTrips
    stop_times <- fetchStopTimes
    Web.Scotty.scotty 4000 $ do
        Web.Scotty.get "/schedules/:route_id" (getRouteScheduleA trips stop_times)


getRouteScheduleA :: (Datastore Trip) -> (Datastore StopTime) -> ActionM ()
getRouteScheduleA all_trips all_stop_times = do
    routeId <- Web.Scotty.param @Text "route_id"
    let routeId' = Data.Text.Short.fromText routeId
    let result = all_trips
                    |> Data.HashMap.Strict.findWithDefault [] routeId'
                    |> fmap (\trip -> tripToResponse all_stop_times trip)
    Web.Scotty.json (result :: Vector ScheduleResponse)


tripToResponse :: Datastore StopTime -> Trip -> ScheduleResponse
tripToResponse all_stop_times trip =
    ScheduleResponse {
        trip_id = trip.trip_id,
        service_id = trip.service_id,
        route_id = trip.route_id,
        schedules = stopTimesFromTrip all_stop_times trip
    }

stopTimesFromTrip :: Datastore StopTime -> Trip -> Vector StopTime
stopTimesFromTrip all_stop_times trip = 
    all_stop_times 
    |> Data.HashMap.Strict.findWithDefault [] trip.trip_id

fetchTrips :: IO (Datastore Trip)
fetchTrips = 
    fetchOrExit "../MBTA_GTFS/trips.txt" (.route_id)

fetchStopTimes :: IO (Datastore StopTime)
fetchStopTimes = 
    fetchOrExit "../MBTA_GTFS/stop_times.txt" (.trip_id)

fetchOrExit :: Data.Csv.FromNamedRecord a => String -> (a -> ShortText) -> IO (Datastore a)
fetchOrExit filename selector = do
    putStrLn ("Fetching " <> filename <> "...")

    contents <- Data.ByteString.Lazy.readFile filename
    case Data.Csv.decodeByName contents of
        Left problem -> do
            putStrLn problem
            System.Exit.exitFailure

        Right (_headers, vals) -> do
            let result = vals |> vecToMap selector
            putStrLn ("Fetching " <> filename <> " done!")
            pure result

vecToMap :: (a -> ShortText) -> Vector a -> Datastore a
vecToMap selector vec =
            vec
            |> Data.Vector.toList
            |> fmap (\val -> (selector val, [val]))
            |> Data.HashMap.Strict.fromListWith (++)
            |> Data.HashMap.Strict.map Data.Vector.fromList