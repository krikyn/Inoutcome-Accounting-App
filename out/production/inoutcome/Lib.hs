{-# LANGUAGE TemplateHaskell #-}
module Lib where

--data TransactionType = Income | Expense | Debt | l deriving (Show, Bounded, Enum)

{-{-instance PersistField TransactionType where
    toPersistValue Income  = toPersistValue "Income"
    toPersistValue Expense = toPersistValue "Expense"
    toPersistValue Debt    = toPersistValue "Debt"
    toPersistValue Asset   = toPersistValue "Asset"
    fromPersistValue a = Income <$> fromPersistValue a-}
instance PersistFieldSql TransactionType where
    sqlType _ = SqlReal-}

{-instance FromHttpApiData TransactionType where
    parseUrlPiece = parseBoundedTextData

$(deriveJSON defaultOptions ''TransactionType)    -}
{-instance ToJSON TransactionType  where
        toJSON (Income a) = toJSON a
        toJSON (Expense a) = toJSON a
        toJSON (Debt a) = toJSON a
        toJSON (Asset a) = toJSON a
instance FromJSON TransactionType where
        parseJSON = Income $ parseRealFloat "Double"  -}
