module Strava (repl, initialStravaContext, getToken) where

import System.Exit
import System.IO.Error
import System.IO
import Network.Curl
import Text.JSON
import Control.Monad()
import Control.Monad.State
import Data.Functor
import Control.Applicative
import Data.Foldable        (traverse_)

stravaUserTokenFile :: String
stravaUserTokenFile = ".strava-token"

clientSecretFile :: String
clientSecretFile = ".client-secret"

clientIdFile :: String
clientIdFile = ".client-id"

data StravaApp = StravaApp
    { clientId      :: String
    , clientSecret  :: String
    }

data StravaAuth = StravaAuth
    { accessToken :: String }

instance JSON StravaAuth where
    readJSON (JSObject o) =
        StravaAuth <$> valFromObj "access_token" o
    readJSON _ = mzero
    showJSON = undefined

data StravaAthlete = StravaAthlete
    { firstname   :: String
    , lastname    :: String
    } deriving (Eq)

instance JSON StravaAthlete where
    readJSON (JSObject o) =
        StravaAthlete                <$>
            valFromObj "firstname" o <*>
            valFromObj "lastname" o
    readJSON _ = mzero
    showJSON = undefined

instance Show StravaAthlete where
    show a = firstname a ++ " " ++ lastname a

data StravaActivity = StravaActivity
    { name                 :: Maybe String
    , athlete              :: Maybe StravaAthlete
    , distance             :: Maybe Double
    , movingTime           :: Maybe Double
    , totalElevationGain   :: Maybe Double
    , startDate            :: Maybe String
    , elapsedTime          :: Maybe Double
    , averageHeartrate     :: Maybe Double
    , maxHeartrate         :: Maybe Double
    } deriving (Eq, Show)

instance JSON StravaActivity where
    readJSON (JSObject o) =
        StravaActivity                      <$>
            queryObj "name"                 <*>
            queryObj "athlete"              <*>
            queryObj "distance"             <*>
            queryObj "moving_time"          <*>
            queryObj "total_elevation_gain" <*>
            queryObj "start_date"           <*>
            queryObj "elapsed_time"         <*>
            queryObj "average_heartrate"    <*>
            queryObj "max_heartrate"
            where queryObj str = case valFromObj str o of
                    (Ok a) -> return $ Just a
                    _      -> return Nothing
    readJSON _ = mzero
    showJSON = undefined

data StravaAPI = StravaAPI
    { listUserActivities :: String
    , listFriendActivities :: String
    }
stravaAPI :: StravaAPI
stravaAPI = StravaAPI
    "https://www.strava.com/api/v3/athlete/activities"
    "https://www.strava.com/api/v3/activities/following"

readStravaAppID :: IO (String, String)
readStravaAppID = do
    i <- readFile clientIdFile
    s <- readFile clientSecretFile
    return (i, s)

authorize :: IO (Maybe StravaAuth)
authorize = do
    checkStravaApp <- tryIOError $ readStravaAppID
    (appId, secret) <- case checkStravaApp of
            Right c -> return c
            Left _ -> do
                putStrLn "Could not read application application id and secret"
                putStrLn "Register your application (http://www.strava.com/developers)"
                putStr "and provide your `application id': "
                hFlush stdout
                i <- getLine
                writeFile clientIdFile i
                putStr "and  your `client secret': "
                hFlush stdout
                s <- getLine
                writeFile clientSecretFile s
                return (i, s)
    let app = StravaApp appId secret
    putStrLn $ "Visit: https://www.strava.com/oauth/authorize?client_id=" ++ (clientId app) ++ "&response_type=code&redirect_uri=http://putigny.net/strava&approval_prompt=force"
    putStrLn "and authorize this application to access your strava data"
    putStr "provide you code: "
    hFlush stdout
    code <- getLine
    (_, curlResp) <- curlGetString
        ("https://www.strava.com/oauth/token?client_id=" ++ (clientId app) ++ "&client_secret=" ++ (clientSecret app) ++ "&code=" ++ code) [CurlPost True]
    let stravaAuth = decode curlResp :: Result StravaAuth
    case stravaAuth of
            Ok a -> writeFile stravaUserTokenFile (accessToken a) >>
                    return (Just $ StravaAuth $ accessToken a)
            _ -> return Nothing

getToken :: IO StravaAuth
getToken = do
    token <- tryIOError $ readFile stravaUserTokenFile -- try to read previous token
    case token of
        Right t -> return $ StravaAuth t
        Left _ -> do
            putStrLn "Could not read previous strava auth token, requesting new token..."
            stravaAuth <- authorize
            case stravaAuth of
                Just auth -> return auth
                Nothing  -> getToken

printActivity :: StravaActivity -> IO ()
printActivity a = do
    printer $ show <$> athlete a
    printer $ name a
    printer $ (\x -> "  " ++ x ++ " km") <$> show <$> (round :: Double -> Int) <$> (/1000) <$> distance a
    printer $ (\x -> "  " ++ x ++ " mD+") <$> show <$> (round :: Double -> Int) <$> totalElevationGain a
    printer $ (\x -> "  HR: " ++ x ++ " (avg)") <$> show <$> (round :: Double -> Int) <$> averageHeartrate a
    printer $ (\x -> "  HR: " ++ x ++ " (max)") <$> show <$> (round :: Double -> Int) <$> maxHeartrate a
    putStr "\n"
    where
        printer = traverse_ putStrLn

lsStrava :: StravaContext ()
lsStrava = do
    aList <- userActivities <$> get
    liftIO $ mapM_ printActivity $ reverse aList

lsFriend :: StravaContext ()
lsFriend = do
    aList <- friendActivities <$> get
    liftIO $ mapM_ printActivity $ reverse aList

data StravaData = StravaData {
    userActivities      ::  [StravaActivity],
    friendActivities    ::  [StravaActivity]
}
initialStravaContext :: StravaData
initialStravaContext = StravaData [] []

type StravaContext a = StateT StravaData IO a
loadStravaContext :: StravaAuth -> StravaContext ()
loadStravaContext sa = do
    (c0, ownJson) <- liftIO $
        curlGetString (listUserActivities stravaAPI) [CurlHttpHeaders [stravaAuth]]
    when (c0 /= CurlOK) $ liftIO $ print c0
    (c1, friendJson) <- liftIO $
        curlGetString (listFriendActivities stravaAPI) [CurlHttpHeaders [stravaAuth]]
    when (c1 /= CurlOK && c1 /= c0) $ liftIO $ print c1
    let aList = decode ownJson :: Result [StravaActivity]
    let a2List = decode friendJson :: Result [StravaActivity]
    let ownAct = case aList of
            Ok a -> a
            _    -> []
    let friendAct = case a2List of
            Ok a -> a
            _    -> []
    put $ StravaData ownAct friendAct
    where stravaAuth = "Authorization: Bearer " ++ accessToken sa

repl :: StravaAuth -> StravaContext ()
repl sa = do
    liftIO $ putStrLn "Loading data from strava"
    loadStravaContext sa
    liftIO $ putStrLn "Done"
    forever $ do
        liftIO $ putStr "> " >> hFlush stdout
        cmd <- liftIO $ tryIOError getLine
        case cmd of
            Right "ls" -> lsStrava
            Right "lf" -> lsFriend
            Right "up" -> loadStravaContext sa
            Right "help" -> liftIO $ do
                putStrLn "Supported commands are:"
                putStrLn "  ls: list user's activities"
                putStrLn "  lf: list friends activities"
                putStrLn "  up: update strava data"
            Right str  -> liftIO $ putStrLn $ str ++ " is not a valid command (use help)"
            Left e     -> liftIO $ if isEOFError e
                                then putStrLn "quit" >> exitSuccess
                                else print e
