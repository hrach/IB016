{- | This is the fourth assignment for IB016, semester spring 2015.
  Name: Jan Skrasek
  UID: 373816

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REQUIRE hjson-query package
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


== Obtaining weather information from http://openweathermap.org

This time your task is to implement download and processing of weather data
from <http://openweathermap.org>. Following it partially filled program, which
already contains some data type definitions, 'main', argument parsing, and
dispatch functions to run your implementation. You should not change any of
already defined data types and functions, unless it is specifically allowed.

OpenWeatherMap provides JSON API for weather forecast (it also provides XML,
but we believe JSON is simpler to process), your task is:

* Download JSON data from serwer based on value of 'Query' (which was parsed
from commandline arguments). That is implement 'createUrl' and
'downloadResults'.

* Process JSON data in functions 'weatherNow', 'weatherDetailed',
'weatherDaily' and 'warmestDay'.

* Implement helper function 'prettyPrint'.

You will have to use some library for HTTP and for JSON, we recommend following
packages and modules @http@ (module @Network.HTTP@) and @hjson@ (module
@Text.HJson@) which provide simple and easy to use interface. Both of these
modules can be installed with @cabal@ (they are not part of standard
distribution). If you use @http@ module you will also need to decode UTF-8
manually, you can use @utf-string@ (module @Codec.Binary.UTF8.String@, function
'decodeString') for that. You can also use any other module which implements
given functionality, in that case you might need to replace 'Json' type in all
functions using it with appropriate type (this change is allowed). Furthermore,
'Rational' data type is used to represent numeric values in weather forecast,
this is to simplify parsing from Json, as @hjson@ uses it to represent numbers,
if you use another library you are allowed to replace Rational with another
type cabable of representing fractional values.

Documentation of OpenWeatherMap's current and forecast API can be found at
<http://openweathermap.org/current> and <http://openweathermap.org/forecast>
respectively, JSON reply format is also linked from appropriate function
documentation. Beware that JSON examples on OpenWeatherMap are not always
properly indented. Furthermore, as a simplification, you can expect that
weather field (which is JSON array in response) contains at least one entry and
you can use the data from the first entry and ignore all other entries.

You can expect that you will obtain a valid JSON from OpenWeatherMap.
If you detect an invalid JSON, you can kill the program using 'exitFailure'
from @System.Exit@ .
However, you should expect that the obtained JSON does not contain all required
information (which might happen if you query an invalid city). For this reason all
JSON-parsing functions you should implement are returning type wrapped with
'WithError a' which is an alias to 'Either String String'. You should emit an
appropriate error message into 'Left' if any JSON field is missing. However, if
you don't feel like it, you can just omit error handling at all which will be
penalised with 5 points (in this case you should just wrap the result into
'Right' to match type declaration).

=== Examples

@
$ ./Weather now --city=Brno
city:        Brno (lat = 49.2, lon = 16.61)
weather:     few clouds
temperature: 2.1 &#xb0;C
pressure:    993.8 hPa

$ ./Weather detailed --city=Brno
city:        Brno (lat = 49.195, lon = 16.608)
date:        05-04-2015 15:00
weather:     scattered clouds
temperature: 2.1 &#xb0;C
pressure:    992.4 hPa
date:        05-04-2015 18:00
weather:     few clouds
temperature: -0.2 &#xb0;C
pressure:    993.8 hPa
date:        05-04-2015 21:00
weather:     light rain
temperature: -2.3 &#xb0;C
pressure:    994.1 hPa
date:        06-04-2015 00:00
weather:     sky is clear
temperature: -3.5 &#xb0;C
pressure:    993.7 hPa
date:        06-04-2015 03:00
weather:     sky is clear
temperature: -3.9 &#xb0;C
pressure:    993.7 hPa
# ...

./Weather daily --city=Brno --count=2
city:        Brno (lat = 49.195, lon = 16.608)
date:        05-04-2015 10:00
weather:     scattered clouds
temperature: 2.1 &#xb0;C
pressure:    992.4 hPa
date:        06-04-2015 10:00
weather:     light snow
temperature: 3.2 &#xb0;C
pressure:    995.0 hPa

$ ./Weather warmest-day --city=Brno
city:        Brno (lat = 49.195, lon = 16.608)
date:        11-04-2015 10:00
weather:     sky is clear
temperature: 16.3 &#xb0;C
pressure:    1006.6 hPa

$ ./Weather warmest-day --city=Brno --count=16
city:        Brno (lat = 49.195, lon = 16.608)
date:        20-04-2015 10:00
weather:     light rain
temperature: 19.8 &#xb0;C
pressure:    988.2 hPa

$ ./Weather now --city="Žďár nad Sázavou"
city:        Žďár nad Sázavou (lat = 49.56, lon = 15.94)
weather:     scattered clouds
temperature: 0.0 &#xb0;C
pressure:    977.4 hPa

$ ./Weather now --coord=49.56,15.94
city:        Zdar nad Sazavou (lat = 49.56, lon = 15.94)
weather:     scattered clouds
temperature: 0.0 &#xb0;C
pressure:    977.4 hPa
@
 -}

module Main (
    -- * Executable entry
      main
    -- * Pre-defined types and functions
    , URL
    , Query (..)
    , QueryType (..)
    , Location (..)
    , parseQuery
    , PrettyPrint (..)
    , disp, disp'
    , City (..)
    , Weather (..)
    , Date (..)
    , valid
    , usage
    , WithError
    , processData
    -- * Required functions and types
    , createUrl
    , downloadResults
    , prettyPrint
    , weatherNow
    , weatherDetailed
    , weatherDaily
    , warmestDay
    ) where

-- for timestamp conversion
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale

import Control.Monad
import Control.Applicative

import Data.Monoid
import Data.Maybe
import Data.List ( sortBy )

import System.Environment
import System.Exit
import System.IO

import Network.HTTP

import Text.HJson
import Text.HJson.Query
import Text.Read ( readMaybe )

import Codec.Binary.UTF8.String ( decodeString )

type URL = String

-- | City location specification
data Location = Name { locName :: String }
              | Coord { lat :: Double, lon :: Double }
              | NoLocation
              deriving ( Eq, Show, Read )

instance Monoid Location where
    mempty = NoLocation
    x `mappend` NoLocation = x
    _ `mappend` y          = y

-- | Type of query
data QueryType = Now | Detailed | Daily | WarmestDay | NotSet
               deriving ( Eq, Show, Read )

instance Monoid QueryType where
    mempty = NotSet
    x `mappend` NotSet = x
    _ `mappend` y      = y

-- | Type representing commandline parameters
data Query = Query { queryType :: QueryType
                   , city :: Location
                   , count :: Last Int
                   }
           deriving ( Eq, Show, Read )

instance Monoid Query where
    mempty = Query mempty mempty mempty
    x `mappend` y = Query { queryType = queryType x `mappend` queryType y
                          , city = city x `mappend` city y
                          , count = count x `mappend` count y
                          }

-- | Parses commandline arguments into 'Query' type
parseQuery :: [String] -> Query
parseQuery [] = mempty
parseQuery (qt:args) = mempty { queryType = qType } `mappend` mconcat (map fromArg args)
  where
    qType = case qt of
        "now"         -> Now
        "daily"       -> Daily
        "detailed"    -> Detailed
        "warmest-day" -> WarmestDay
        _             -> NotSet

    fromArg :: String -> Query
    fromArg arg = fromMaybe mempty $ do
        (k, '=':v) <- Just $ span (/= '=') arg -- if pattern fail we get Nothing from this do block
        case k of
            "--city"  -> Just $ mempty { city = Name v }
            "--coord" -> do
                (slat, ',':slon) <- Just $ span (/= ',') v
                lat <- readMaybe slat
                lon <- readMaybe slon
                Just $ mempty { city = Coord { lat = lat, lon = lon } }
            "--count" -> do
                cnt <- readMaybe v
                return $ mempty { count = Last (Just cnt) }
            _ -> Nothing

-- | A type class to facilitate pretty printing of tablular information.
class PrettyPrint a where
    -- | Format object into list of key-value pairs of string representation.
    -- This is later used by 'prettyPrint' to format data for output.
    ppKeyVal :: a -> [(String, String)]

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b) where
    ppKeyVal (x, y) = ppKeyVal x ++ ppKeyVal y

instance (PrettyPrint a, PrettyPrint b, PrettyPrint c) => PrettyPrint (a, b, c) where
    ppKeyVal (x, y, z) = ppKeyVal x ++ ppKeyVal y ++ ppKeyVal z

instance PrettyPrint a => PrettyPrint [a] where
    ppKeyVal = concatMap ppKeyVal

-- | Should format data which can have key-value representation into well
-- readable tabular form. That is all the form key: value, with value indented
-- such that all values start at same offset. Key-value pairs should be obtained by
-- 'ppKeyVal' function from 'PrettyPrint' class.
--
-- >>> putStrLn $ prettyPrint (Date 0)
-- date: 01-01-1970 00:00
--
-- >>>  putStrLn $ prettyPrint (Date 0, Weather 0 0 "test")
-- date:        01-01-1970 00:00
-- weather:     test
-- temperature: -273.2 °C
-- pressure:    0.0 hPa
--
prettyPrint :: PrettyPrint a => a -> String
prettyPrint a = unlines $ map (\(k,v) -> take max (k ++ ":" ++ repeat ' ') ++ v) pairs
    where
        max = (+2) . maximum $ map (length . fst) pairs
        pairs = ppKeyVal a

-- | Show rational number rounded with given precision
--
-- >>> disp 1 1.007
-- "1.0"
--
-- >>> disp 2 1.007
-- "1.01"
--
-- >>> disp 3 2.2
-- "2.2"
disp :: Int -> Rational -> String
disp n = show . (/ 10^n) . fromIntegral . round . (* 10^n)

-- | Shortcut for @'disp' 1@.
disp' :: Rational -> String
disp' = disp 1

-- | Information about city and its location.
data City = City { cityName :: String
                 , cityLat  :: Rational
                 , cityLon  :: Rational
                 } deriving ( Eq, Show, Read )

instance PrettyPrint City where
    ppKeyVal c = [ ("city", cityName c ++ " (lat = " ++
                            disp 3 (cityLat c) ++ ", lon = " ++
                            disp 3 (cityLon c) ++ ")") ]

-- | Information about weather.
data Weather = Weather { temperature :: Rational
                       , pressure    :: Rational
                       , description :: String
                       } deriving ( Eq, Show, Read )

instance PrettyPrint Weather where
    ppKeyVal w = [ ("weather", description w)
                 , ("temperature", disp' (temperature w - 273.15) ++ " °C")
                 , ("pressure", disp' (pressure w) ++ " hPa")
                 ]

-- | Unix time wrapped so that it can be made instance of 'PrettyPrint'.
newtype Date = Date { timestamp :: Rational }
               deriving ( Eq, Show, Read )

instance PrettyPrint Date where
    ppKeyVal (Date d) = [ ("date", formatTime defaultTimeLocale "%d-%m-%Y %R" unixTime) ]
      where
        unixTime = posixSecondsToUTCTime (realToFrac d)

-- | Create URL from given query, that is add in all parameters necessary to
-- obtain weather data.
--
-- It must properly encode all parameters for example using functions from
-- @Network.HTTP.Base@.
--
-- >>> createUrl $ mempty {queryType = Now, city = Name "Brno" }
-- "http://api.openweathermap.org/data/2.5/weather?q=Brno"
--
-- >>>  createUrl $ mempty {queryType = Detailed, city = Name "Brno" }
-- "http://api.openweathermap.org/data/2.5/forecast?q=Brno"
--
-- >>> createUrl $ mempty {queryType = Daily, city = Name "Brno" }
-- "http://api.openweathermap.org/data/2.5/forecast/daily?q=Brno"
--
-- >>> createUrl $ mempty {queryType = WarmestDay, city = Name "Brno", count = Last (Just 5) }
-- "http://api.openweathermap.org/data/2.5/forecast/daily?q=Brno&cnt=5"
--
-- >>> createUrl $ mempty {queryType = Now, city = Name "Žďár nad Sázavou" }
-- "http://api.openweathermap.org/data/2.5/weather?q=%C5%BD%C4%8F%C3%A1r%20nad%20S%C3%A1zavou"
createUrl :: Query -> URL
createUrl q = base ++ qtype q ++ urlEncodeVars (args q)
   where
        base = "http://api.openweathermap.org/data/2.5/"
        qtype q = case queryType q of
            Now         -> "weather?"
            Detailed    -> "forecast?"
            Daily       -> "forecast/daily?"
            WarmestDay  -> "forecast/daily?"
            NotSet      -> error "Query type is not set"
        args q = pushCount q $ pushPlace q []
        pushPlace q a = a ++ case city q of
            Name n -> [("q", n)]
            Coord lat lon -> [("lat", disp 3 $ toRational lat),  ("lon", disp 3 $ toRational lon)]
            _ -> []
        pushCount q a = a ++ case getLast $ count q of
            Just n -> [("cnt", show n)]
            _ -> []

-- | Download requested URL and parse JSON out of it.
--
-- It is recommended to use functionality of @Network.HTTP@ for download
-- and @Text.HJson@ for JSON representation and parsing. If you use HTTP
-- library which does not handle unicode (such as @Network.HTTP@), you should
-- decode response manualy using 'decodeString' from @Codec.Binary.UTF8.String@.
downloadResults :: URL -> IO Json
downloadResults url = do
    response <- simpleHTTP $ getRequest url
    body <- getResponseBody response
    let json = fromString $ decodeString body
    case json of
        Left a -> exitFailure
        Right a -> return a

-- | Check validity of 'Query'.
valid :: Query -> Bool
valid q = queryType q /= NotSet && city q /= NoLocation

-- | Program usage.
usage :: String
usage = unlines [
    "Usage: Weather {now|detailed|daily|warmest-day}",
    "               {--city=CITY | --coord=LATITUDE,LONGITUDE} [--count=CNT]",
    "",
    "    --count  applies only to daily and warmest-day and specifies number of days"
    ]

-- | Alias to 'Either' to simplify types.
type WithError a = Either String a

-- | Parse current weather from JSON,
-- see <http://openweathermap.org/weather-data#current> for format description.
weatherNow :: Json -> WithError (City, Weather)
weatherNow json
    | isNothing cityName = Left "missing key city name"
    | isNothing cityLat  = Left "missing key city lat"
    | isNothing cityLon  = Left "missing key city lon"
    | isNothing temp     = Left "missing key temp"
    | isNothing press    = Left "missing key press"
    | isNothing desc     = Left "missing key desc"
    | otherwise          = Right (
        City (fromJust cityName) (toRational $ fromJust cityLat) (toRational $ fromJust cityLon),
        Weather (toRational $ fromJust temp) (toRational $ fromJust press) (fromJust desc)
    )
    where
        cityName = exec json [getFromKey "name"] :: Maybe String
        cityLat  = exec json [getFromKey "coord" >>> getFromKey "lat"] :: Maybe Double
        cityLon  = exec json [getFromKey "coord" >>> getFromKey "lon"] :: Maybe Double
        temp     = exec json [getFromKey "main"  >>> getFromKey "temp"] :: Maybe Double
        press    = exec json [getFromKey "main"  >>> getFromKey "pressure"] :: Maybe Double
        desc     = exec json [getFromKey "weather" >>> getFromIndex 0 >>> getFromKey "description"] :: Maybe String


exec :: Jsonable a => Json -> [JFilter] -> Maybe a
exec json filters = if null result then Nothing else head result
    where
        filter f = if null res then Nothing else fromJson $ head res where res = f json
        filtered = map filter filters
        result   = dropWhile isNothing filtered


-- | Parse detailed (5-day/3 hour) forecast from JSON,
-- see <http://openweathermap.org/weather-data#5days> for format specification.
weatherDetailed :: Json -> WithError (City, [(Date, Weather)])
weatherDetailed json = case maybe_city of
        Left message -> Left message
        Right city   -> Right (city, weatherDayList json)
    where
        maybe_city = weatherCity json


-- | Parse daily (16-day) forecast from JSON,
-- see <http://openweathermap.org/weather-data#16:maindays> for format specification.
-- Note: you should use temperature from @day@ temperature entry.
weatherDaily :: Json -> WithError (City, [(Date, Weather)])
weatherDaily json = case maybe_city of
        Left message -> Left message
        Right city   -> Right (city, weatherDayList json)
    where maybe_city = weatherCity json


weatherCity :: Json -> WithError City
weatherCity json
    | isNothing cityName = Left "missing key city name"
    | isNothing cityLat  = Left "missing key city lat"
    | isNothing cityLon  = Left "missing key city lon"
    | otherwise          = Right $ City (fromJust cityName) (toRational $ fromJust cityLat) (toRational $ fromJust cityLon)
    where
        cityName = exec json [getFromKey "city" >>> getFromKey "name"] :: Maybe String
        cityLat  = exec json [getFromKey "city" >>> getFromKey "coord" >>> getFromKey "lat"] :: Maybe Double
        cityLon  = exec json [getFromKey "city" >>> getFromKey "coord" >>> getFromKey "lon"] :: Maybe Double


weatherDayList :: Json -> [(Date, Weather)]
weatherDayList json = mapMaybe weatherDay jsons
    where jsons = (getFromKey "list" >>> getFromArr) json


weatherDay :: Json -> Maybe (Date, Weather)
weatherDay json
    | isNothing date || isNothing temp || isNothing press || isNothing desc = Nothing
    | otherwise = Just (
        Date (toRational $ fromJust date),
        Weather (toRational $ fromJust temp) (toRational $ fromJust press) (fromJust desc)
    )
    where
        date  = exec json [getFromKey "dt"] :: Maybe Double
        temp  = exec json [getFromKey "temp"  >>> getFromKey "day"
                           ,getFromKey "main"  >>> getFromKey "temp"] :: Maybe Double
        press = exec json [getFromKey "pressure"
                           ,getFromKey "main" >>> getFromKey "pressure"] :: Maybe Double
        desc  = exec json [getFromKey "weather" >>> getFromIndex 0 >>> getFromKey "description"] :: Maybe String


-- | Parse daily forecast and get warmest day from it.
warmestDay :: Json -> WithError (City, Date, Weather)
warmestDay json = case maybe_city of
        Left message -> Left message
        Right city   -> Right (city, fst top, snd top)
    where
        maybe_city = weatherCity json
        list = weatherDayList json
        sorted = sortBy (\(_, w1@(Weather t1 _ _)) (_, w2@(Weather t2 _ _)) -> if t1 < t2 then GT else (if t1 == t2 then EQ else LT)) list
        top = head sorted


-- | Dispatch parsing functionas based on 'QueryType' and handle errors.
processData :: QueryType -> Json -> String
processData qtype json = either handler id $ case qtype of
    Now        -> prettyPrint <$> weatherNow json
    Detailed   -> prettyPrint <$> weatherDetailed json
    Daily      -> prettyPrint <$> weatherDaily json
    WarmestDay -> prettyPrint <$> warmestDay json
    _          -> Left "invalid query"
  where
    handler msg = unlines [ "Error processing data, sorry", msg]

main :: IO ()
main = do
    query <- parseQuery <$> getArgs
    unless (valid query) $ do
        hPutStrLn stderr "Invalid options"
        hPutStrLn stderr usage
        exitFailure
    weather <- downloadResults (createUrl query)
    putStrLn $ processData (queryType query) weather
