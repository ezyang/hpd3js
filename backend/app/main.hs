import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)
import qualified Network.Wai.Handler.FastCGI as FastCGI
import Control.Monad

-- NB: This is not the default runner!

main :: IO ()
main = fromArgs parseExtra >>= makeApplication >>= FastCGI.run . fst
