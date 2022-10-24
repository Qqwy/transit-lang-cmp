module Main (main) where

import System.Exit qualified
import Data.Csv qualified
import Data.List qualified
import Data.Vector (Vector)
import Data.Vector qualified
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified
import Data.Function (on)
import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.ByteString.Lazy qualified
import Web.Scotty qualified
import Common ((|>))
import Lib (Trip(..), StopTime(..))

main :: IO ()
main = do
    trips <- fetchTrips
    stop_times <- fetchStopTimes
    Web.Scotty.scotty 3000 $ do
        Web.Scotty.get "/schedules/:route_id" getRouteScheduleA


getRouteScheduleA :: Action
getRouteScheduleA = do
    route_id <- Web.Scotty.param "route_id"
    json []



fetchTrips :: IO (HashMap ByteString (Vector Trip))
fetchTrips = 
    fetchOrExit "../MBTA_GTFS/trips.txt" (.route_id)

fetchStopTimes :: IO (HashMap ByteString (Vector StopTime))
fetchStopTimes = 
    fetchOrExit "../MBTA_GTFS/stop_times.txt" (.trip_id)

fetchOrExit :: Data.Csv.FromNamedRecord a => String -> (a -> ByteString) -> IO (HashMap ByteString (Vector a))
fetchOrExit filename selector = do
    contents <- Data.ByteString.Lazy.readFile filename
    case Data.Csv.decodeByName contents of
        Left problem -> do
            putStrLn problem
            System.Exit.exitFailure

        Right (_headers, vals) ->
            vals
            |> vecToMap selector
            |> pure

vecToMap :: (a -> ByteString) -> Vector a -> HashMap ByteString (Vector a)
vecToMap selector vec =
            vec
            |> Data.Vector.toList
            |> fmap (\val -> (selector val, [val]))
            |> Data.HashMap.Strict.fromListWith (++)
            |> Data.HashMap.Strict.map Data.Vector.fromList