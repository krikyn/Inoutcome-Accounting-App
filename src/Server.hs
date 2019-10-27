{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Server
  ( main )
  where

import Control.Monad.Logger
import Data.Int (Int64)
import Data.Maybe

import Database.Persist hiding (get)
import Database.Persist.Sql (fromSqlKey)
import Database.Persist.Sqlite hiding (get)
import Web.Spock
import Web.Spock.Config

import Web.Spock.Lucid (lucid)

import Model.Schema
import Network.Wai.Middleware.Static
import View.Account
import View.SignIn
import View.SignUp
import View.Charts
import Control.Monad.Cont (liftIO)
import Data.Time
import View.AccountModule.DBMaintenance
import Lucid.Base (toHtmlRaw)

type CurrentSession = Maybe Int64

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "main.db" 5
  runStdoutLoggingT $ runSqlPool (runMigration mainSchema) pool
  cfg <- defaultSpockCfg (Nothing :: CurrentSession) (PCPool pool) ()
  runSpock 8087 (spock cfg app)

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

insertUserAndReturnKey :: User -> SqlPersistT (LoggingT IO) Int64
insertUserAndReturnKey user = fromSqlKey <$> insert user

insertTransactionAndReturnKey :: Transaction -> SqlPersistT (LoggingT IO) Int64
insertTransactionAndReturnKey transaction = fromSqlKey <$> insert transaction

fetchUser :: User -> SqlPersistT (LoggingT IO) (Maybe Int64)
fetchUser (User email password) = (fmap (fromSqlKey . entityKey)) <$>
  (listToMaybe <$> selectList [UserEmail ==. email, UserPassword ==. password] [])

fetchUserById :: Int64 -> SqlPersistT (LoggingT IO) (Maybe (Entity User))
fetchUserById userId = selectFirst [UserId ==. (toSqlKey userId)] []

app :: SpockM SqlBackend CurrentSession () ()
app =
    do middleware (staticPolicy (addBase "static"))
       get root $ redirect "account"
       get "signin" $ do
           maybeUserId <- readSession
           case maybeUserId of
               Just _ -> redirect "account"
               Nothing -> lucid $ makeSignInPage ""
       get "signup" $ lucid $ makeSignUpPage ""
       post "signin" $ do
           e <- param "email"
           p <- param "password"
           case (e, p) of
             (Nothing, _) -> lucid $ makeSignInPage "Email field cannot be empty"
             (Just "", _) -> lucid $ makeSignInPage "Email field cannot be empty"
             (_, Nothing) -> lucid $ makeSignInPage "Password field cannot be empty"
             (_, Just "") -> lucid $ makeSignInPage "Password field cannot be empty"
             (Just email, Just password) -> do
                 userIdMaybe <- runSQL $ fetchUser $ User email password
                 case userIdMaybe of
                   Nothing     -> lucid $ makeSignInPage "The email or password is incorrect"
                   Just userId -> writeSession (Just userId) >> redirect "account"
       post "signup" $ do
            maybeUserId <- readSession
            case maybeUserId of
                Just _ -> redirect "account"
                Nothing -> do
                    e <- param "email"
                    p <- param "password"
                    cp <- param "confirmpassword"
                    case (e, p, cp) of
                        (Nothing, _, _) -> lucid $ makeSignUpPage "Email field cannot be empty"
                        (Just "", _, _) -> lucid $ makeSignUpPage "Email field cannot be empty"
                        (_, Nothing, _) -> lucid $ makeSignUpPage "Password field cannot be empty"
                        (_, Just "", _) -> lucid $ makeSignUpPage "Password field cannot be empty"
                        (_, _, Nothing) -> lucid $ makeSignUpPage "Confirm password field cannot be empty"
                        (_, _, Just "") -> lucid $ makeSignUpPage "Confirm password field cannot be empty"
                        (Just email, Just password, Just cPassword) -> do
                            if password /= cPassword
                            then lucid $ makeSignUpPage "Passwords do not match"
                            else do
                              userIdMaybe <- runSQL $ fetchUser $ User email password
                              case userIdMaybe of
                                  Nothing -> do
                                    userId <- runSQL $ insertUserAndReturnKey (User email password)
                                    writeSession (Just userId)
                                    redirect "account"
                                  Just _ -> lucid $ makeSignUpPage "Selected address is already registered"
       get "account" $ do
           maybeUserId <- readSession
           case maybeUserId of
                 Nothing -> redirect "signin"
                 Just userId  -> do
                     maybeUser <- runSQL $ fetchUserById userId
                     transactions <- runSQL $ selectList [TransactionBelongsTo ==. (toSqlKey userId)] [Desc TransactionId]
                     case maybeUser of
                         Just user -> do
                             localPath <- liftIO currentDBFile
                             archivedDBPaths <- liftIO listArchivedDatabases
                             let accountState = parseToAccountState userId user transactions localPath archivedDBPaths
                             liftIO $ generateGeneralCharts accountState
                             lucid $ getAccountPage accountState
                         Nothing -> lucid $ makeSignInPage "Unexpected error, try connecting later"
       post "account" $ do
           mTransactionType <- param "transactionType"
           mAmount <- param "amount"
           mDescription <- param "description"
           mUserId <- readSession
           now <- liftIO getCurrentTime
           case (mUserId, mTransactionType, mAmount, mDescription) of
               (Just userId, Just transactionType, Just amount, Just description) -> do
                   _ <- runSQL $ insertTransactionAndReturnKey (Transaction (toSqlKey userId) transactionType amount description now)
                   redirect "account"
               (Nothing, _, _, _) -> redirect "signin"
               (_, _, _, _) -> redirect "account"
       get "logout" $ do
         writeSession Nothing
         redirect "signin"
       get "createChart" $ do
         liftIO $ createCircleDiagram "unic" "General statistic" []
         redirect "signin"
       get "db" $ do
         dbs <- liftIO $ listArchivedDatabases
         lucid $ toHtmlRaw (show dbs)
       post "db" $ do
          a <- param "action"
          p <- param "path"
          case (a, p) of
            (Nothing, _) -> redirect "account"
            (_, Nothing) -> redirect "account"
            (Just "deploy", Just param) -> do
                liftIO $ deployDBFile param
                redirect "account"
            (Just "backup", _) -> do
                liftIO backupLocalDBFile
                redirect "account"
            (Just "archive", _) -> do
                liftIO backupAndDeployEmpty
                redirect "account"

