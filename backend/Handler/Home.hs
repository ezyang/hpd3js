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

dateTimeFormat :: String
dateTimeFormat = "%e %b %Y %H:%M:%S"

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

getViewR :: ProfileId -> Handler RepHtmlJson
getViewR pid = do
    profile <- runDB $ get404 pid
    -- XXX ick ick
    let path = "static" </> uploadDirectory </> Text.unpack (profileHash profile)
        dlpath = "/" ++ path
    let html = do
            setTitle . toHtml $ profileTitle profile
            [whamlet|<h1>#{profileTitle profile}
                     <p>
                        Hash is #{profileHash profile} uploaded on #{format (profileTime profile)}.
                        <a href=#{dlpath}>Download.
                        |]
        -- Since paths are by hash, this is always the same value
        -- assuming the parse process is deterministic.  This won't work
        -- well if database/filesystem get out of sync.  We also
        -- validated the profile so that it would definitely parse
        -- properly.
        Just pdata = unsafePerformIO (readProfile path)
        buildSeries (cid, samples) = object [ "label" .= (IntMap.!) (Prof.prNames pdata) cid
                                            , "data" .= array samples
                                            ]
        addSampleData time xs (cid, cost) = IntMap.insertWith' (++) cid [(time, cost)] xs
        addSample :: IntMap [(Prof.Time, Prof.Cost)] -> (Prof.Time, Prof.ProfileSample) -> IntMap [(Prof.Time, Prof.Cost)]
        addSample xs (time, sample) = foldl' (addSampleData time) xs sample
        json = object [ "data" .= array (map buildSeries (IntMap.toList (foldl' addSample IntMap.empty (Prof.prSamples pdata))))
                      , "samples" .= array (map snd (Prof.prSamples pdata))]
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
                Just _ -> do
                    uploadId <- runDB $ do
                        existing <- getBy (UniqueHash hash)
                        case existing of
                            Nothing -> insert (Profile title hash currentTime)
                            Just (Entity pid _) -> return pid
                    redirect (ViewR uploadId)
        _ -> defaultLayout $ do
            setTitle "Unable to upload file"
            [whamlet|<h1>Unable to upload file|]

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Upload a heap profile:"
    <*> areq textField "Description:" Nothing
