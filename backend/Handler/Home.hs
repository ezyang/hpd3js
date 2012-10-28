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

dateTimeFormat :: String
dateTimeFormat = "%e %b %Y %H:%M:%S"

format :: UTCTime -> String
format = formatTime defaultTimeLocale dateTimeFormat

-- no 'staticFiles' splice, since the contents of 'uploaded' will always
-- be dynamic

uploadDirectory :: FilePath
uploadDirectory = "uploaded"

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    profiles <- runDB $ selectList [] [Desc ProfileTime, LimitTo 10]
    let handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "hp/d3.js"
        $(widgetFile "homepage")

getViewR :: ProfileId -> Handler RepHtml
getViewR pid = do
    profile <- runDB $ get404 pid
    defaultLayout $ do
        setTitle . toHtml $ profileTitle profile
        [whamlet|<h1>#{profileTitle profile}
                 <p>Hash is #{profileHash profile} uploaded on #{format (profileTime profile)}|]

postUploadR :: Handler RepHtml
postUploadR = do
    ((result, _), _) <- runFormPost sampleForm
    case result of
        FormSuccess (file, title) -> do
            -- XXX Need to check if the file is legit to avoid spamming
            bhash :: SHA1 <- liftIO . runResourceT $ fileSource file $$ sinkHash
            let hash = Text.decodeUtf8 . Base16.encode $ encode bhash
            _ <- liftIO $ evaluate hash
            currentTime <- liftIO getCurrentTime
            liftIO $ fileMove file (uploadDirectory </> Text.unpack hash)
            uploadId <- runDB . insert $ Profile title hash currentTime
            redirect (ViewR uploadId)
        _ -> defaultLayout $ do
            setTitle "Unable to upload file"
            [whamlet|<h1>Unable to upload file|]

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Upload a heap profile:"
    <*> areq textField "Description:" Nothing
