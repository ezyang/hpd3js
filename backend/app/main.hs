import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)
import Network.Wai.Handler.FastCGI (run)
import Control.Monad

-- main :: IO ()
-- main = defaultMain (fromArgs parseExtra) makeApplication

main :: IO ()
main = fromArgs parseExtra >>= makeApplication >>= run
