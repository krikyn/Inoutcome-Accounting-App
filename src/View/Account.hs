{-# LANGUAGE OverloadedStrings    #-}
module View.Account ( parseToAccountState
                    , generateGeneralCharts
                    , getAccountPage
                    ) where

import Lucid
import Model.Schema
import Control.Monad.Cont (forM_)
import Database.Persist
import Data.Text (pack)
import Data.Int (Int64)
import qualified Data.Text as T
import View.Charts
import Data.Time
import View.AccountModule.DBMaintenance

type NamedParameter = (String, Double)

data AccountState = AccountState
  {  accId           :: String
  ,  email           :: T.Text
  ,  transactions    :: [Transaction]
  ,  localPath       :: FilePath
  ,  archivedDBPaths :: [FilePath]
  ,  parameters      :: [NamedParameter]
  ,  components      :: [NamedParameter]
  } deriving (Show)

findByNameOrReturnZero :: String -> [NamedParameter] -> Double
findByNameOrReturnZero _ [] = 0
findByNameOrReturnZero name ((str, dbl) : xs)
    | name == str = dbl
    | otherwise   = findByNameOrReturnZero name xs

parseToAccountState :: Int64 -> Entity User -> [Entity Transaction] -> FilePath -> [FilePath] -> AccountState
parseToAccountState userId (Entity _ user) transactionsEntity localPath archivedDBPaths = do
  let trns = map entityVal transactionsEntity
      calcTotal name = sum $ map transactionAmount $ filter (\t -> transactionTransactionType t == name) trns
      income = calcTotal "income"
      expense = calcTotal "expense"
      debt = calcTotal "debt"
      asset = calcTotal "asset"
  AccountState (show userId) (userEmail user) trns localPath archivedDBPaths  [ ("inflows", income)
                                                                               , ("activity", income - expense + asset)
                                                                               , ("available", income - expense)
                                                                               , ("budgeted", income - expense + asset - debt)
                                                                               ]
                                                                               [ ("income", income)
                                                                               , ("expense", expense)
                                                                               , ("debt", debt)
                                                                               , ("asset", asset)
                                                                               ]

makeTransactionDiv :: Transaction -> Html ()
makeTransactionDiv transaction = do
    div_ [ class_ "transaction" ] $ do
        let getHtmlField field = toHtml $ field transaction
            rawType = transactionTransactionType transaction
            sign transactionType = case transactionType of
                "income" -> "+"
                "expense" -> "-"
                "debt" -> "-"
                "asset" -> "+"
                _ -> ""
        div_ [ class_ $ (pack "transaction_amount " <> rawType) ] $ toHtml $ (sign rawType  ++ (show $ transactionAmount transaction))
        div_ [ class_ "transaction_type" ] $ getHtmlField transactionTransactionType
        div_ [ class_ "transaction_description" ] $ getHtmlField transactionDescription


type ModuleParam = (String, String, String)

makeModuleWithChart :: ModuleParam -> Html ()
makeModuleWithChart (unicId, tabName, fileName) = do
  button_ [ type_ "button", class_ "extra-module" ] $ toHtml ("🞃 " ++ tabName)
  div_ [ class_ "module-content" ] $ img_ [ src_ $ pack ("img/" ++ unicId ++ fileName ++ ".svg"), width_ "600", height_ "600" ]

getAccountPage :: AccountState -> Html ()
getAccountPage state = assembleAccountPage state

transformWithAccumSum :: Double -> Double -> [Transaction] -> [(UTCTime, Double, Double)]
transformWithAccumSum _ _ [] = []
transformWithAccumSum accIncome accExpense ((Transaction _ "income" amount _ date) : xs) = (date, amount+accIncome, accExpense) : (transformWithAccumSum (amount+accIncome) accExpense xs)
transformWithAccumSum accIncome accExpense ((Transaction _ "expense" amount _ date) : xs) = (date, accIncome, amount+accExpense) : (transformWithAccumSum accIncome (amount+accExpense) xs)
transformWithAccumSum accIncome accExpense (_ : xs) = transformWithAccumSum accIncome accExpense xs

generateGeneralCharts :: AccountState -> IO ()
generateGeneralCharts accountState = do
  createCircleDiagram' "Key metrics statistics" parameters
  createCircleDiagram' "Component Statistics" components
  createCrackChart (accId accountState) "Incomes and  expenses chart" incomesAndExpenses
  where valuesOf category = map (\(a,b) -> (a, b, False)) (category accountState)
        createCircleDiagram' title category = createCircleDiagram (accId accountState) title (valuesOf category)
        incomesAndExpenses = transformWithAccumSum 0 0 (reverse $ transactions accountState)

assembleAccountPage :: AccountState -> Html ()
assembleAccountPage accountState = do
    html_ $ do
        head_ $ do
            meta_ [ charset_ "UTF-8" ]
            title_ "Account"
            link_ [ rel_ "stylesheet", href_ "css/account.css" ]
        body_ $ do
            link_ [ href_ "https://fonts.googleapis.com/css?family=Open+Sans:400,300,700", rel_ "stylesheet", type_ "text/css" ]
            h1_ [ class_ "page-name" ] $ "My account"
            a_ [ class_ "user-sign", href_ "account" ] $ toHtml (email accountState)
            a_ [ class_ "logout2", href_ "logout" ] $ "Logout"
            div_ [ class_ "top-bar" ] $ do
                let showRoubles amount = toHtml (pack (show amount) <> " ₽")
                    getParam name = findByNameOrReturnZero name (parameters accountState)
                h2_ [ class_ "top-bar_header" ] $ "General statistics"
                div_ [ class_ "top-bar_common-container" ] $ do
                    div_ [ class_ "top-bar_container" ] $ do
                        label_ [ class_ "top-bar_field-name" ] $ "Total budgeted"
                        p_ [ class_ "top-bar_field-value" ] $ showRoubles $ getParam "budgeted"
                    div_ [ class_ "top-bar_container" ] $ do
                        label_ [ class_ "top-bar_field-name" ] $ "Total activity"
                        p_ [ class_ "top-bar_field-value" ] $ showRoubles $ getParam "activity"
                    div_ [ class_ "top-bar_container" ] $ do
                        label_ [ class_ "top-bar_field-name" ] $ a_ [ class_ "underlined" ] $ "Total available"
                        p_ [ class_ "top-bar_field-value" ] $ showRoubles $ getParam "available"
                    div_ [ class_ "top-bar_container" ] $ do
                        label_ [ class_ "top-bar_field-name" ] $ "Total inflows"
                        p_ [ class_ "top-bar_field-value" ] $ showRoubles $ getParam "inflows"
            let accId' = accId accountState
            mapM_ (makeModuleWithChart . (\ (a, b) -> (accId', a, b)))
              [("Key metrics statistics", "Keymetricsstatistics"),
               ("Component Statistics", "ComponentStatistics"),
               ("Comparative graph of income and expenses","Incomesandexpenseschart")]
            dbMaintenanceModule (localPath accountState) (archivedDBPaths accountState)
            div_ [ class_ "transactions-list" ] $ do
                h2_ [ class_ "transactions-list_header" ] $ "Transactions"
                forM_ (transactions accountState) makeTransactionDiv
            div_ [ class_ "form-container" ] $ form_ [ class_ "add-transaction-form", action_ "", method_ "post", name_ "form" ] $ do
                div_ [ class_ "label-input-container" ] $ do
                    label_ [ class_ "form-label", for_ "email" ] $ "Transaction type"
                    select_ [ class_ "form-styling transaction-type", name_ "transactionType" ] $ do
                        option_ [ value_ "income" ] $ "Income"
                        option_ [ value_ "expense" ] $ "Expense "
                        option_ [ value_ "debt" ] $ "Debt"
                        option_ [ value_ "asset" ] $ "Asset"
                div_ [ class_ "label-input-container" ] $ do
                    label_ [ class_ "form-label", for_ "email" ] $ "Amount"
                    input_ [ class_ "form-styling amount", type_ "text", name_ "amount", placeholder_ "" ]
                div_ [ class_ "label-input-container description-input-container" ] $ do
                    label_ [ class_ "form-label", for_ "email" ] $ "Description"
                    input_ [ class_ "form-styling description", type_ "text", name_ "description", placeholder_ "" ]
                div_ [ class_ "label-input-container button" ] $ div_ [ class_ "button-animate", onclick_ "document.forms['form'].submit();"] $ a_ [ class_ "button-label" ] $ "＋ Add"
        script_ "\n    var coll = document.getElementsByClassName(\"extra-module\");\n    var i;\n    for (i = 0; i < coll.length; i++) {\n      coll[i].addEventListener(\"click\", function() {\n        this.classList.toggle(\"active\");\n        var content = this.nextElementSibling;\n        if (content.style.display === \"block\") {\n          content.style.display = \"none\";\n        } else {\n          content.style.display = \"block\";\n        }\n      });\n    }\n  "