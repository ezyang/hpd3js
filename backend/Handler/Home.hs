{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Home where

import Import
import Crypto.Conduit
import Crypto.Hash.SHA1 (SHA1)
import Data.Conduit
import Data.Serialize
import Control.Exception (evaluate)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Base16 as Base16
import System.FilePath
import qualified Data.Text as Text
import Data.Time
import System.Locale (defaultTimeLocale)
import System.Directory
import Profiling.Heap.Read (readProfile)
import qualified Profiling.Heap.Types as Prof
import System.IO.Unsafe
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List (foldl')
import Yesod.Static
import Data.Time.Git

-- XXX Gotta do history

dateTimeFormat :: String
dateTimeFormat = "%B %e, %Y %H:%M:%S"

uploadDirectory :: FilePath
uploadDirectory = "uploaded"

format :: UTCTime -> String
format = formatTime defaultTimeLocale dateTimeFormat

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    profiles <- runDB $ selectList [] [Desc ProfileTime, LimitTo 10]
    let handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "hp/d3.js"
        $(widgetFile "homepage")

getViewR :: Hash -> Handler RepHtmlJson
getViewR hash = do
    Entity _ profile <- runDB $ getBy404 (UniqueHash hash)
    -- XXX Text versus string, what a PAIN IN THE ASS
    let components = [uploadDirectory, Text.unpack (unHash hash)]
        path = "static" </> foldr1 (</>) components
        lpath = StaticR (StaticRoute (map Text.pack components) [])
    let html = do
            let showreel = "http://ezyang.github.com/hpd3js/showreel/showreel.html#" ++ Text.unpack (unHash hash)
            setTitle . toHtml $ profileTitle profile
            [whamlet|
                     <h1>
                       <a href=@{HomeR}>
                         #{profileTitle profile}
                     <div>
                       <code>#{profileJob profile}</code>
                     <div>
                       #{format (profileRunTime profile)} (uploaded #{format (profileTime profile)}); <a href=@{lpath}>download .hp</a>
                     <iframe width="100%" height="650px" src=#{showreel} />
                        |]
        -- Since paths are by hash, this is always the same value
        -- assuming the parse process is deterministic.  This won't work
        -- well if database/filesystem get out of sync.  We also
        -- validated the profile so that it would definitely parse
        -- properly.
        Just pdata = unsafePerformIO (readProfile path)
        buildSeries (cid, samples) = object [ "key" .= (IntMap.!) (Prof.prNames pdata) cid
                                            , "data" .= array samples
                                            ]
        fx [new] [] = [new]
        fx [(time', cost')] old@((time, cost):xs) | time' == time = (time, cost + cost'):xs
                                                       | otherwise     = (time', cost'):old
        fx _ _ = error "Invariant broken on fx"
        addSampleData time xs (cid, cost) = IntMap.insertWith fx cid [(time, cost)] xs
        addSample :: IntMap [(Prof.Time, Prof.Cost)] -> (Prof.Time, Prof.ProfileSample) -> IntMap [(Prof.Time, Prof.Cost)]
        addSample xs (time, sample) = foldl' (addSampleData time) xs sample
        json = object [ "data" .= array (map buildSeries (IntMap.toList (foldl' addSample IntMap.empty (Prof.prSamples pdata))))
                      , "samples" .= array (reverse (map fst (Prof.prSamples pdata)))]
    -- Watch out, no sensitive data allowed!
    setHeader "Access-Control-Allow-Origin" "*"
    defaultLayoutJson html json

postUploadR :: Handler RepHtml
postUploadR = do
    ((result, _), _) <- runFormPost sampleForm
    case result of
        FormSuccess (file, title) -> do
            bhash :: SHA1 <- liftIO . runResourceT $ fileSource file $$ sinkHash
            let hash = Text.decodeUtf8 . Base16.encode $ encode bhash
            _ <- liftIO $ evaluate hash
            currentTime <- liftIO getCurrentTime
            let path = "static" </> uploadDirectory </> Text.unpack hash
            liftIO $ fileMove file path
            parse <- liftIO $ readProfile path
            case parse of
                Nothing -> do
                    liftIO $ removeFile path
                    defaultLayout $ do
                        setTitle "Not a valid heap profile"
                        [whamlet|<h1>Not a valid heap profile|]
                Just profile -> do
                    let job = Text.pack (Prof.prJob profile)
                        runTime = maybe currentTime posixToUTC (approxidate (Prof.prDate profile))
                    _ <- runDB $ do
                        existing <- getBy (UniqueHash (Hash hash))
                        case existing of
                            Nothing -> insert (Profile title (Hash hash) job currentTime runTime)
                            Just (Entity pid _) -> return pid
                    redirect (ViewR (Hash hash))
        FormFailure es -> defaultLayout $ do
            setTitle "Failure"
            [whamlet|<h1>Failure
                     <ul>
                       $forall e <- es
                         <li>#{e}
            |]
        FormMissing -> defaultLayout $ do
            setTitle "Missing data"
            [whamlet|<h1>Missing data|]

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Upload a heap profile:"
    <*> areq textField "Description:" Nothing
