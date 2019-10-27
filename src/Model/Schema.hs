{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model.Schema where

import qualified Data.Text as T
import Database.Persist.TH
import Data.Time.Clock (UTCTime)

share [mkPersist sqlSettings, mkMigrate "mainSchema"] [persistLowerCase|
User json
  email           T.Text
  password        T.Text
  deriving        Show
Transaction json
  belongsTo       UserId
  transactionType T.Text
  amount          Double
  description     T.Text
  date            UTCTime
  deriving Show
|]
