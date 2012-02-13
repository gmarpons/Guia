module Guia.Db.BillingConcept
    (BillingConcept(..), itemToStrings, validStrings, billingConceptFinalPrice,
     finalPrice, basePriceToString, vatRatioToString, finalPriceToString,
     fetchAllBillingConcepts
    )
where

import Database.HDBC
    (handleSqlError, prepare, execute, fetchAllRows, fromSql, toSql, commit,
     SqlValue, fetchRow)
import Database.HDBC.PostgreSQL 
    (Connection)
import Numeric
    (showFFloat)

import Guia.Db.Item
import Guia.Db.DataUtils

data BillingConcept = 
    BillingConcept 
    { billingConceptId :: Integer -- Created by DB
    , description :: String
    , basePrice :: Double
    , vatRatio :: Double
    } deriving (Show)

instance ItemClass BillingConcept where
    itemToStrings = billingConceptToStrings
    updateItemOnDb = updateBillingConceptOnDb
    insertItemFromStringsOnDb = insertBillingConceptFromStringsOnDb
    deleteItemOnDb = deleteBillingConceptOnDb                                  

billingConceptFinalPrice :: BillingConcept -> Double
billingConceptFinalPrice bc = finalPrice (basePrice bc) (vatRatio bc)

finalPrice :: Double -> Double -> Double
finalPrice basePrice vatRatio = basePrice + (basePrice * vatRatio)

billingConceptToStrings :: BillingConcept -> [String]
billingConceptToStrings bc = map ($ bc) [ description
                                        , basePriceToString
                                        , vatRatioToString
                                        , finalPriceToString ]

localChecks :: [String -> Bool]
localChecks = [ (> 0) . length
              , isNumeric
              , isNumeric
              , const True ]

globalCheck :: [String] -> Bool
globalCheck = const True

validStrings :: [String] -> Bool
validStrings = genericValidStrings localChecks globalCheck

basePriceToString :: BillingConcept -> String
basePriceToString = flip (showFFloat (Just 2)) "" . basePrice

vatRatioToString :: BillingConcept -> String
vatRatioToString = flip (showFFloat (Just 2)) "" . vatRatio

finalPriceToString :: BillingConcept -> String
finalPriceToString = flip (showFFloat (Just 2)) "" . billingConceptFinalPrice

fetchAllBillingConcepts :: Connection -> IO [BillingConcept]
fetchAllBillingConcepts conn =
    handleSqlError $ do
      stmt <- prepare conn "SELECT * FROM billing_concepts"
      execute stmt []
      results <- fetchAllRows stmt
      return (map convertBillingConceptRow results)

convertBillingConceptRow :: [SqlValue] -> BillingConcept
convertBillingConceptRow [billingConceptId, description, basePrice, vatRatio] = 
           BillingConcept { billingConceptId = ((fromSql billingConceptId)::Integer)
                          , description = ((fromSql description)::String)
                          , basePrice = ((fromSql basePrice)::Double)
                          , vatRatio = ((fromSql vatRatio)::Double)
                          }

-- | This functiona assumes that strings for @newBasePrice@ and
-- @newVatRatio@ are parsable into Doubles.
insertBillingConceptFromStringsOnDb :: [String] -> Connection -> IO BillingConcept
insertBillingConceptFromStringsOnDb [ description
                                    , basePrice
                                    , vatRatio
                                    , _finalPrice
                                    ] conn =
    handleSqlError $ do
      -- TODO: simplify this func using INSERT ... RETURNING
      insertStmt <- prepare conn 
                     "INSERT INTO billing_concepts \
                     \(description, base_price, vat_ratio) VALUES (?, ?, ?)"
      modifiedCount <- execute insertStmt [ toSql description
                                          , toSql ((read basePrice) :: Double)
                                          , toSql ((read vatRatio) :: Double)]
      putStrLn $ show modifiedCount
      maxStmt <- prepare conn
                     "SELECT MAX(id) FROM billing_concepts"
      execute maxStmt []
      Just maxId <- fetchRow maxStmt
      fetchStmt <- prepare conn "SELECT * FROM billing_concepts WHERE id = ?"
      execute fetchStmt maxId
      Just maxIdRow <- fetchRow fetchStmt
      commit conn
      return $ convertBillingConceptRow maxIdRow

updateBillingConceptOnDb :: [String] 
                         -> BillingConcept 
                         -> Connection 
                         -> IO BillingConcept
updateBillingConceptOnDb stringsNew old conn =
    handleSqlError $ do
      let new = updateBillingConceptFromStrings stringsNew old
      stmt <- prepare conn 
                          "UPDATE billing_concepts SET description = ?, \
                          \base_price = ?, vat_ratio = ?  WHERE id = ?"
      modifiedCount <- execute stmt [ toSql (description new)
                                    , toSql (basePrice new)
                                    , toSql (vatRatio new)
                                    , toSql (billingConceptId new)]
      putStrLn $ show modifiedCount
      commit conn
      return new

-- | This functiona assumes that strings for @newBasePrice@ and
-- @newVatRatio@ are parsable into Doubles.
updateBillingConceptFromStrings :: [String] -> BillingConcept -> BillingConcept
updateBillingConceptFromStrings [ newDescription
                                , newBasePrice
                                , newVatRatio
                                , _newFinalPrice
                                ] bc
    = bc { description = newDescription
         , basePrice = read newBasePrice
         , vatRatio = read newVatRatio }

deleteBillingConceptOnDb :: BillingConcept -> Connection -> IO ()
deleteBillingConceptOnDb bc conn = 
    handleSqlError $ do
      stmt <- prepare conn 
                     "DELETE FROM billing_concepts WHERE id = ?"
      modifiedCount <- execute stmt [toSql (billingConceptId bc)]
      putStrLn $ show modifiedCount
      commit conn
