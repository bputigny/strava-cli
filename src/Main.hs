import Control.Monad.State
import Strava

main :: IO ()
main = do
    auth <- getToken
    evalStateT (repl auth) initialStravaContext
