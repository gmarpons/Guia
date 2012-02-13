module Guia.Db.Invoicing
where

import Database.HDBC
-- 
import Database.HDBC.PostgreSQL
    (Connection)
import Numeric
    (showFFloat)
import Data.Time.Calendar
    (Day, toGregorian)
import Data.Time.Clock
    (getCurrentTime, utctDay)
import System.Locale
    (defaultTimeLocale, TimeLocale(..))

import Guia.Db.Item

data Invoicing = Invoicing
    { invoicingId :: Integer    -- Created by DB
    , invoicingDescription :: String
    , issuingDate :: Day
    } deriving (Show)

instance ItemClass Invoicing where
    itemToStrings = invoicingToStrings
    updateItemOnDb = updateInvoicingOnDb
    insertItemFromStringsOnDb = insertInvoicingFromStringsOnDb
    deleteItemOnDb = deleteInvoicingOnDb

data Charge = Charge
    { chargeId :: Integer       -- Created by DB
    , chargePayerId :: Integer
    , chargeInvoiceId :: Integer
    , chargeInvoicingId :: Integer
    , chargeLastName :: String
    , actualDescription :: String
    , actualBasePrice :: Double
    , actualVatRatio :: Double
    } deriving (Show)

invoicingToStrings :: Invoicing -> [String]
invoicingToStrings inv = map ($ inv) [ invoicingDescription
                                     , show . issuingDate
                                     , totalBasePriceToString
                                     , totalFinalPriceToString
                                     , numberOfInvoicesToString
                                     ]

totalBasePrice :: Invoicing -> Double
totalBasePrice = const 0.0

totalFinalPrice :: Invoicing -> Double
totalFinalPrice = const 0.0

numberOfInvoices :: Invoicing -> Integer
numberOfInvoices = const 0

totalBasePriceToString :: Invoicing -> String
totalBasePriceToString = const ""

totalFinalPriceToString :: Invoicing -> String
totalFinalPriceToString = const ""

numberOfInvoicesToString :: Invoicing -> String
numberOfInvoicesToString = const ""

localChecks :: [String -> Bool]
localChecks = [ const True
              , const True ]

globalCheck :: [String] -> Bool
globalCheck = const True

validStrings :: [String] -> Bool
validStrings = genericValidStrings localChecks globalCheck

actualBasePriceToString :: Charge -> String
actualBasePriceToString = flip (showFFloat (Just 2)) "" . actualBasePrice

actualVatRatioToString :: Charge -> String
actualVatRatioToString = flip (showFFloat (Just 2)) "" . actualVatRatio

chargeFinalPrice :: Charge -> Double
chargeFinalPrice c = finalPrice (actualBasePrice c) (actualVatRatio c)

chargeVatAmount :: Charge -> Double
chargeVatAmount c = (actualBasePrice c) * (actualVatRatio c)

-- TODO: Replicated in BillingConcepts.hs!!
finalPrice :: Double -> Double -> Double
finalPrice basePrice vatRatio = basePrice + (basePrice * vatRatio)

fetchAllInvoicings :: Connection -> IO [Invoicing]
fetchAllInvoicings conn =
    handleSqlError $ do
      stmt <- prepare conn
                     "SELECT id, description, issuing_date \
                     \FROM   invoicings \
                     \ORDER BY issuing_date DESC"
      execute stmt []
      results <- fetchAllRows stmt
      return (map convertInvoicingRow results)

convertInvoicingRow :: [SqlValue] -> Invoicing
convertInvoicingRow [ invoicingId, invoicingDescription, issuingDate ] =
    Invoicing { invoicingId = ((fromSql invoicingId) :: Integer)
              , invoicingDescription = ((fromSql invoicingDescription) :: String)
              , issuingDate = ((fromSql issuingDate) :: Day)
              }

fetchChargesOfAnInvoicing :: Invoicing -> Connection -> IO [Charge]
fetchChargesOfAnInvoicing invoicing conn =
    handleSqlError $ do
      stmt <- prepare conn
              "SELECT c.id, p.id, i.id, i.invoicing_id_is_included_in, \
              \       n.last, c.actual_description, c.actual_base_price, \
              \       c.actual_vat_ratio \
              \FROM   payers p, person_names n, charges c, invoices i \
              \WHERE  c.invoice_id_is_part_of = i.id and \
              \       i.invoicing_id_is_included_in = ? and p.name_id = n.id and \
              \       i.payer_id_is_charged_to = p.id and p.id >= 000"
      execute stmt [toSql (invoicingId invoicing)]
      results <- fetchAllRows stmt
      return (map convertChargeRow results)

-- TODO: almost identical code than the previous one
fetchCharge :: Integer -> Connection -> IO Charge
fetchCharge id conn =
    handleSqlError $ do
      stmt <- prepare conn
              "SELECT c.id, p.id, i.id, i.invoicing_id_is_included_in, \
              \       n.last, c.actual_description, c.actual_base_price, \
              \       c.actual_vat_ratio \
              \FROM   payers p, person_names n, charges c, invoices i \
              \WHERE  c.id = ? AND \
              \       c.invoice_id_is_part_of = i.id AND \
              \       p.name_id = n.id AND i.payer_id_is_charged_to = p.id"
      execute stmt [toSql id]
      Just chargeRow <- fetchRow stmt
      return $ convertChargeRow chargeRow

convertChargeRow :: [SqlValue] -> Charge
convertChargeRow [ chargeId, chargePayerId, chargeInvoiceId, chargeInvoicingId
                 , lastName, actualDescription, actualBasePrice
                 , actualVatRatio] =
    Charge { chargeId = ((fromSql chargeId) :: Integer)
           , chargePayerId = ((fromSql chargePayerId) :: Integer)
           , chargeInvoiceId = ((fromSql chargeInvoiceId) :: Integer)
           , chargeInvoicingId = ((fromSql chargeInvoicingId) :: Integer)
           , chargeLastName = ((fromSql lastName) :: String)
           , actualDescription = ((fromSql actualDescription) :: String)
           , actualBasePrice = ((fromSql actualBasePrice) :: Double)
           , actualVatRatio = ((fromSql actualVatRatio) :: Double)
           }

-- Stub: invoicings cannot be deleted
deleteInvoicingOnDb :: Invoicing -> Connection -> IO ()
deleteInvoicingOnDb _ _ = return ()

deleteChargeOnDb :: Charge -> Connection -> IO ()
deleteChargeOnDb charge conn =
    handleSqlError $ do
      deleteChargeStmt <- prepare conn 
                     "DELETE FROM charges WHERE id = ?"
      modifiedCount1 <- execute deleteChargeStmt [toSql (chargeId charge)]
      putStrLn $ show modifiedCount1
      deleteInvoiceStmt <- prepare conn
                     "DELETE FROM invoices \
                     \WHERE id = ? AND NOT EXISTS \
                     \(SELECT * FROM charges WHERE invoice_id_is_part_of = ?)"
      modifiedCount2 <- execute deleteInvoiceStmt $
                        replicate 2 (toSql (chargeInvoiceId charge))
      putStrLn $ show modifiedCount2
      commit conn
      return ()

cloneInvoicingOnDb :: [Charge] -> Connection -> IO Invoicing
cloneInvoicingOnDb charges conn = do
  (newDescription, _) <- invoicingNewDescription conn
  newInvoicing <- insertInvoicingFromStringsOnDb [newDescription] conn
  mapM_ (cloneChargeOnDb newInvoicing conn) charges
  return newInvoicing

cloneChargeOnDb :: Invoicing -> Connection -> Charge -> IO ()
cloneChargeOnDb invoicing conn oldCharge = do
  insertChargeOnDb (invoicingId invoicing) (chargePayerId oldCharge)
                       (actualDescription oldCharge) (actualBasePrice oldCharge)
                       (actualVatRatio oldCharge) conn
  return ()

insertInvoicingFromStringsOnDb :: [String] -> Connection -> IO Invoicing
insertInvoicingFromStringsOnDb (description:_) conn =
    handleSqlError $ do
      stmt <- prepare conn
                     "INSERT INTO invoicings \
                     \(id, description, issuing_date) \
                     \VALUES (DEFAULT, ?, DEFAULT) \
                     \RETURNING id, description, issuing_date"
      modifiedCount <- execute stmt $ [toSql description]
      putStrLn $ show modifiedCount
      Just invoicingRow <- fetchRow stmt
      commit conn
      return $ convertInvoicingRow invoicingRow

insertChargeOnDb :: Integer 
                 -> Integer
                 -> String
                 -> Double
                 -> Double
                 -> Connection 
                 -> IO Charge
insertChargeOnDb chargeInvoicingId
                 chargePayerId
                 actualDescription
                 actualBasePrice
                 actualVatRatio
                 conn = do
    handleSqlError $ do
      queryInvoiceStmt <- prepare conn
                     "SELECT id \
                     \FROM invoices \
                     \WHERE invoicing_id_is_included_in = ? \
                     \      AND payer_id_is_charged_to = ?"
      execute queryInvoiceStmt $ [toSql chargeInvoicingId, toSql chargePayerId]
      rows <- fetchAllRows queryInvoiceStmt
      chargeInvoiceId <- if length rows == 0
        then do
          queryAccountStmt <- prepare conn
                     "SELECT bank_id, office, control_digits, num \
                     \FROM payers p, bank_accounts a \
                     \WHERE p.bank_account_id = a.id AND p.id = ?"
          execute queryAccountStmt $ [toSql chargePayerId]
          Just accountRow <- fetchRow queryAccountStmt
          let actualBankAccount = concat (map fromSql accountRow) :: String
          insertInvoiceStmt <- prepare conn
                     "INSERT INTO invoices \
                     \(id, payer_id_is_charged_to, invoicing_id_is_included_in, \
                     \ actual_bank_account) \
                     \VALUES (DEFAULT, ?, ?, ?) \
                     \RETURNING id"
          modifiedCount2 <- execute insertInvoiceStmt $ [ toSql chargePayerId
                                                        , toSql chargeInvoicingId
                                                        , toSql actualBankAccount ]
          putStrLn $ show modifiedCount2
          Just invoiceIdRow <- fetchRow insertInvoiceStmt
          return invoiceIdRow
        else do
          return $ head rows
      insertChargeStmt <- prepare conn 
                     "INSERT INTO charges \
                     \(id, actual_description, actual_base_price, \
                     \ actual_vat_ratio, invoice_id_is_part_of) \
                     \VALUES (DEFAULT, ?, ?, ?, ?) \
                     \RETURNING id"
      modifiedCount1 <- execute insertChargeStmt [ toSql actualDescription
                                                 , toSql actualBasePrice
                                                 , toSql actualVatRatio
                                                 , head chargeInvoiceId
                                                 ]
      putStrLn $ show modifiedCount1
      Just chargeIdRow <- fetchRow insertChargeStmt
      charge <- fetchCharge ((fromSql (head chargeIdRow)) :: Integer) conn
      commit conn
      return charge

updateInvoicingOnDb :: [String] -> Invoicing -> Connection -> IO Invoicing
updateInvoicingOnDb _ oldInvoicing conn =
    handleSqlError $ do
      stmt <- prepare conn 
                     "UPDATE invoicings \
                     \SET issuing_date = DEFAULT \
                     \WHERE id = ? \
                     \RETURNING id, description, issuing_date"
      modifiedCount <- execute stmt [toSql (invoicingId oldInvoicing)]
      putStrLn $ show modifiedCount
      Just invoicingRow <- fetchRow stmt
      commit conn
      return $ convertInvoicingRow invoicingRow

-- | This description generation method only works if descriptions are
-- not editable by user.
invoicingNewDescription :: Connection -> IO (String, Day)
invoicingNewDescription conn =
    handleSqlError $ do
      currentTime <- getCurrentTime
      let today = utctDay currentTime
          (currentYear, currentMonth, _) = toGregorian today
          currentMonthString = fst (months defaultTimeLocale !! (currentMonth - 1))
          descriptionPrefix = currentMonthString ++ " " ++ show currentYear
      stmt <- prepare conn
                     "SELECT count(*) \
                     \FROM invoicings \
                     \WHERE description LIKE ? || '%'"
      execute stmt [toSql descriptionPrefix]
      Just count <- fetchRow stmt
      let numRows = (fromSql (head count)) :: Int
          description = if numRows == 0 then descriptionPrefix
                        else descriptionPrefix ++ " (" ++ show (numRows + 1) ++ ")"
      return (description, today)
