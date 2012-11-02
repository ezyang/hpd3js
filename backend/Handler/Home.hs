{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Home where

import System.FilePath
import System.Locale (defaultTimeLocale)
import System.Directory
import System.IO.Unsafe

import Control.Exception (evaluate)

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap) -- not strict
import Data.List (foldl')
import Data.Time
import Data.String
import Data.Function
import Data.Maybe
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text
import Data.Conduit
import Data.Serialize

import Import
import Yesod.Static

import Crypto.Conduit
import Crypto.Hash.SHA1 (SHA1)
import Profiling.Heap.Read (readProfile)
import qualified Profiling.Heap.Types as Prof
import Data.Time.Git

-- XXX Gotta do history

uploadDirectory :: IsString a => a
uploadDirectory = "uploaded"

format :: UTCTime -> String
format = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC"

-- Unsafe IO so that the results can be lazy.  Since it's by hash,
-- the contents are invariant, making this pretty safe.  We
-- assume that forcing pdata is sufficient to close the file
-- descriptor; otherwise we have an fd leak!
loadProfile :: Hash -> Prof.Profile
loadProfile hash = fromMaybe (error "Bad profile in store!") (unsafePerformIO (readProfile path))
    where path = "static" </> uploadDirectory </> Text.unpack (unHash hash)

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost uploadForm
    profiles <- runDB $ selectList [] [Desc ProfileUploadTime, LimitTo 10]
    let handlerName = "getHomeR" :: Text
    defaultLayout $ do
        setTitle "hp/d3.js"
        $(widgetFile "homepage")

getViewR :: Hash -> Handler RepHtmlJson
getViewR hash = do
    Entity pid profile <- runDB $ getBy404 (UniqueHash hash)
    annotations <- runDB $ selectList [AnnotationProfileId ==. pid] [Asc AnnotationCostCenter]
    let lpath    = StaticR (StaticRoute [uploadDirectory, unHash hash] [])
        -- Iframe is temporary hack before we merge the two codebases
        showreel = "http://ezyang.github.com/hpd3js/showreel/showreel.html#" ++ Text.unpack (unHash hash)
        sliceAnnot (Entity _ a) = (annotationCostCenter a,
                                  [object ["tix" .= annotationTimeIndex a, "text" .= annotationText a]])
        annotMap = IntMap.fromAscListWith (++) (map sliceAnnot annotations)
    let html = do
            setTitle . toHtml $ profileTitle profile
            [whamlet|
                     <h1>
                       <a href=@{HomeR}>
                         \#{profileTitle profile}
                     <div>
                       <code>#{profileJob profile}</code>
                     <div>
                       \#{format (profileRunTime profile)} (uploaded #{format (profileUploadTime profile)});
                       <a href=@{lpath}>download .hp</a>
                     <div>
                       \#{profileDescription profile} (<a href=@{EditR hash}>Edit</a>)
                     <iframe width="100%" height="600px" frameborder="0" src=#{showreel} />
                        |]
        pdata = loadProfile hash
        buildSeries (cid, samples) = object [ "cid"  .= cid
                                            , "values" .= array (map makePoint (insertMissing samples timetable))
                                            , "annotations" .= array (IntMap.findWithDefault [] cid annotMap)
                                            ]
        makePoint y = object ["y" .= y]
        -- Compares an association list with a key list and fills in
        -- missing data with zeros.
        insertMissing :: (Eq k, Num a) => [(k, a)] -> [k] -> [a]
        insertMissing [] ys = map (const 0) ys
        insertMissing (_:_) [] = error "insertMissing: Impossible, ran out of times"
        insertMissing ((t,x):xs) (y:ys) | t == y    = x : insertMissing xs ys
                                        | otherwise = 0 : insertMissing ((t,x):xs) ys

        -- Combines adjacent list entries with the same index
        fx [new] [] = [new]
        fx [(time', cost')] old@((time, cost):xs) | time' == time = (time, cost + cost'):xs
                                                  | otherwise     = (time', cost'):old
        fx _ _ = error "Invariant broken on fx"

        addSampleData time xs (cid, cost) = IntMap.insertWith fx cid [(time, cost)] xs
        addSample :: IntMap [(Prof.Time, Prof.Cost)] -> (Prof.Time, Prof.ProfileSample) -> IntMap [(Prof.Time, Prof.Cost)]
        addSample xs (time, sample) = foldl' (addSampleData time) xs sample

        timetable = reverse (map fst (Prof.prSamples pdata))
        json = object [ "data"      .= array (map buildSeries (IntMap.toList (foldl' addSample IntMap.empty (Prof.prSamples pdata))))
                      , "timetable" .= array timetable
                      , "nametable" .= array (makeContiguous 0 (IntMap.toAscList (Prof.prNames pdata)))
                      ]
    -- Watch out, no sensitive data allowed!
    setHeader "Access-Control-Allow-Origin" "*"
    defaultLayoutJson html json

-- Invariant: input list must be ascending and distinct.
-- Given j, arranges that element (i, x) in the input is Just x in
-- the (i-j)th position of the output list.  If i is missing from
-- the input puts in Nothing.
makeContiguous :: Int -> [(Int, a)] -> [Maybe a]
makeContiguous _ [] = []
makeContiguous k o@((i, x):xs) | k == i    = Just x  : makeContiguous (k+1) xs
                               | otherwise = Nothing : makeContiguous (k+1) o

editForm :: Profile -> Form (Text, Textarea)
editForm profile = renderDivs $ (,)
    <$> areq textField "Title:" (Just (profileTitle profile))
    <*> areq textareaField "Description:" (Just (profileDescription profile))

getEditR :: Hash -> Handler RepHtml
getEditR hash = do
    Entity _ profile <- runDB $ getBy404 (UniqueHash hash)
    (formWidget, formEnctype) <- generateFormPost (editForm profile)
    defaultLayout $ do
        setTitle "Edit"
        [whamlet|
            <div #form>
              <form method=post action=@{EditR hash}#form enctype=#{formEnctype}>
                ^{formWidget}
                <input type="submit" value="Submit">
            |]

handleForm :: Yesod a => FormResult t -> (t -> GHandler sub a RepHtml) -> GHandler sub a RepHtml
handleForm result f = case result of
    FormSuccess r -> f r
    FormFailure es -> defaultLayout $ do
        setTitle "Failure"
        [whamlet|
             <h1>Failure
             <ul>
               $forall e <- es
                 <li>#{e}
        |]
    FormMissing -> defaultLayout $ do
        setTitle "Missing data"
        [whamlet|<h1>Missing data|]

postEditR :: Hash -> Handler RepHtml
postEditR hash = do
    Entity pid profile <- runDB $ getBy404 (UniqueHash hash)
    ((result, _), _) <- runFormPost (editForm profile)
    handleForm result $ \(title, description) -> do
    runDB $ update pid [ProfileTitle =. title, ProfileDescription =. description]
    redirect (ViewR hash)

getAnnotateR :: Hash -> Handler RepHtml
getAnnotateR hash = do
    Entity _ _ <- runDB $ getBy404 (UniqueHash hash)
    (formWidget, formEnctype) <- generateFormPost (annotateForm hash)
    defaultLayout $ do
        setTitle "Add annotation to #{profileTitle profile}"
        [whamlet|
            <div #form>
              <form method=post action=@{AnnotateR hash}#form enctype=#{formEnctype}>
                ^{formWidget}
                <input type="submit" value="Submit">
            |]


postAnnotateR :: Hash -> Handler RepHtml
postAnnotateR hash = do
    Entity pid _ <- runDB $ getBy404 (UniqueHash hash)
    ((result, _), _) <- runFormPostNoToken (annotateForm hash) -- XXX temporary no token
    handleForm result $ \(cc, time, text) -> do
    -- XXX blah, this should be combinator-ified
    if Text.null text
        then do
            _ <- runDB $ deleteBy (UniqueAnnotation pid cc time)
            return ()
        else do
            old <- runDB $ getBy (UniqueAnnotation pid cc time)
            case old of
                Nothing -> do
                    _ <- runDB $ insert (Annotation pid cc time text)
                    return ()
                Just (Entity aid _) -> do
                    runDB $ update aid [AnnotationText =. text]
                    return ()
    redirect (ViewR hash)

annotateForm :: Hash -> Form (Int, Int, Text)
annotateForm hash = renderDivs $ (,,)
    <$> areq (check validateCostCenter intField) "Cost center:" Nothing
    <*> areq (check validateTimeIndex  intField) "Time index:"  Nothing
    <*> areq textField "Text:" Nothing
    where validateCostCenter x | IntMap.member x (Prof.prNames pdata) = Right x
                               | otherwise = Left ("Unknown cost-center" :: Text)
          validateTimeIndex  x | x < 0 = Left ("Time index cannot be negative" :: Text)
                               | x >= length (Prof.prSamples pdata) = Left ("Time index out of bounds" :: Text)
                               | otherwise = Right x
          pdata = loadProfile hash

postUploadR :: Handler RepHtml
postUploadR = do
    ((result, _), _) <- runFormPost uploadForm
    handleForm result $ \(file, title) -> do
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
                    Nothing -> insert (Profile title (Hash hash) job (Textarea "") currentTime runTime)
                    Just (Entity pid _) -> return pid
            redirect (ViewR (Hash hash))

uploadForm :: Form (FileInfo, Text)
uploadForm = renderDivs $ (,)
    <$> fileAFormReq "Upload a heap profile:"
    <*> areq textField "Title:" Nothing
