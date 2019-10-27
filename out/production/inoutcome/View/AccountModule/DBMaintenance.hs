{-# LANGUAGE OverloadedStrings #-}
module View.AccountModule.DBMaintenance ( listArchivedDatabases
                                        , dbMaintenanceModule
                                        , currentDBFile
                                        , backupLocalDBFile
                                        , backupAndDeployEmpty
                                        , deployDBFile
                                        ) where

import Control.Monad.Cont (forM)
import Data.Text (pack)
import Lucid
import System.Directory
import System.FilePath
import Data.Time

storageDirName :: FilePath
storageDirName = "inoutcomes"

defaultDBFileName :: String
defaultDBFileName = "main.db"

unknownDate :: String
unknownDate = "&emsp;&emsp;—&emsp;&emsp;"

deployDBFile :: FilePath -> IO ()
deployDBFile db = do
  appDir <- getCurrentDirectory
  let distFile = appDir </> "main.db"
  copyFile db distFile

backupAndDeployEmpty :: IO ()
backupAndDeployEmpty = do
  appDir <- getCurrentDirectory
  let emptyFile = appDir </> "static" </> "db" </> "main.db"
      distFile = appDir </> "main.db"
  copyFile emptyFile distFile

backupLocalDBFile :: IO ()
backupLocalDBFile = do
  current <- currentDBFile
  print current
  homeDir <- getHomeDirectory
  print homeDir
  t <- getZonedTime
  let destination = homeDir </> storageDirName </> "main_" ++ (formatTime defaultTimeLocale "%m.%d.%Y"  t) ++ ".db"
  print destination
  copyFile current destination

currentDBFile :: IO FilePath
currentDBFile = do
  dir <- getCurrentDirectory
  maybeFile <- findFile [dir] defaultDBFileName
  case maybeFile of
    Just file -> return file
    Nothing   -> return defaultDBFileName

listArchivedDatabases :: IO [FilePath]
listArchivedDatabases = do
  homeDir <- getHomeDirectory
  let storageDir = homeDir </> storageDirName
  exist <- doesDirectoryExist (homeDir </> storageDirName)
  if exist
    then
      findAllFilesWithExt storageDir "db"
    else do
      createDirectory storageDir
      return []

findAllFilesWithExt :: FilePath -> String -> IO [FilePath]
findAllFilesWithExt dir ext = do
  contents <- listDirectory dir
  paths <- forM contents $ \fileWithExt -> do
    let path = dir </> fileWithExt
    exist <- doesFileExist path
    if exist
      then return $ [path | (isExtensionOf ext) path]
      else findAllFilesWithExt path ext
  return $ concat paths

dbLines :: FilePath -> Html ()
dbLines path = do
  div_ [ class_ "module-line" ] $ do
    div_ [ class_ "module-line_gray-circle" ] $ mempty
    let date = drop 5 $ takeBaseName path
    div_ [ class_ "module-line_date" ] $ toHtmlRaw (if length date == 10 then date else unknownDate)
    div_ [ class_ "module-line_discription" ] $ toHtmlRaw path
    form_ [ class_ "module-line-form", action_ "db", method_ "post" ] $ do
      input_ [ type_ "hidden", name_ "action", value_ "deploy" ]
      input_ [ type_ "hidden", name_ "path", value_ (pack path) ]
      input_ [ class_ "module-line_button", type_ "submit", name_ "button-value", value_ "Deploy" ]

dbMaintenanceModule :: FilePath -> [FilePath] ->  Html ()
dbMaintenanceModule current archived = do
  button_ [ type_ "button", class_ "extra-module" ] $ "🞃 Database maintenance"
  div_ [ class_ "module-content" ] $ do
      h2_ [ class_ "module_header" ] $ "Current Database"
      div_ [ class_ "module-line" ] $ do
          div_ [ class_ "module-line_green-circle" ] $ mempty
          div_ [ class_ "module-line_date" ] $ toHtmlRaw unknownDate
          div_ [ class_ "module-line_discription" ] $ toHtmlRaw current
          form_ [ class_ "module-line-form", action_ "db", method_ "post" ] $ do
              input_ [ type_ "hidden", name_ "action", value_ "backup" ]
              input_ [ type_ "hidden", name_ "path", value_ (pack current) ]
              input_ [ class_ "module-line_button", type_ "submit", name_ "button-value", value_ "Make backup copy" ]
          form_ [ class_ "module-line-form", action_ "db", method_ "post" ] $ do
              input_ [ type_ "hidden", name_ "action", value_ "archive" ]
              input_ [ type_ "hidden", name_ "path", value_ (pack current) ]
              input_ [ class_ "module-line_button", type_ "submit", name_ "button-value", value_ "Archive and deploy empty" ]
      h2_ [ class_ "module_header" ] $ "Available archived Databases"
      mapM_ dbLines archived
