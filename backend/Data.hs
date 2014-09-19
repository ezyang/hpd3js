{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Quasi
import Database.Persist.Sql

newtype Hash = Hash { unHash :: Text }
    deriving (Show, Read, Eq, PathPiece, PersistField, PersistFieldSql)
