module Guia.Db.Payer
    (Payer(..), itemToStrings, validStrings, BankAccount(..),
     bankIdToString, officeToString, controlDigitsToString, numToString,
     bankAccountToString, lastAndFirstName,
     fetchAllPayers, convertBankAccountRow
    )
where

import Database.HDBC
-- 
import Database.HDBC.PostgreSQL
    (Connection)
import Data.Time.Calendar
    (Day)
import Data.Maybe
    (fromMaybe)
import Data.Char
    (isDigit, digitToInt, intToDigit)
import Data.List
    (concatMap)

import Guia.Db.Item

data Payer = Payer
    { payerId :: Integer        -- Created by DB
    , registrationDate :: Day   -- Default created by DB
    , firstName :: String
    , lastName :: String
    , mBankAccount :: Maybe BankAccount
    } deriving (Show)

instance ItemClass Payer where
    itemToStrings = payerToStrings
    updateItemOnDb = updatePayerOnDb
    insertItemFromStringsOnDb = insertPayerFromStringsOnDb
    deleteItemOnDb = deletePayerOnDb                                  

data BankAccount = BankAccount
    { bankAccountId :: Integer  -- Created by DB
    , bankId :: String
    , office :: String
    , controlDigits :: String
    , num :: String
    } deriving (Show)

payerToStrings :: Payer -> [String]
payerToStrings payer = map ($ payer) [ firstName
                                     , lastName
                                     , bankIdToString
                                     , officeToString
                                     , controlDigitsToString
                                     , numToString ]
localChecks :: [String -> Bool]
localChecks = [ const True
              , (> 0) . length
              ] ++ repeat (const True)

globalCheck :: [String] -> Bool
globalCheck strings@[firstName, lastName, bankId, office, controlDigits, num] = 
    -- "**" not allowed as control digits in new bank accounts.
    let accountStrings = tail (tail strings) 
        [fstControlDigit, sndControlDigit] = controlDigits in
    (all null accountStrings) 
    || ((all (all isDigit) accountStrings)
        && (and $ zipWith (==) (map length accountStrings) [4, 4, 2, 10])
        && (isCorrect fstControlDigit (bankId ++ office))
        && (isCorrect sndControlDigit num))

isCorrect :: Char -> String -> Bool
isCorrect controlDigit word = controlDigit == computedDigit
    where weights :: [Int]
          weights = [6, 3, 7, 9, 10, 5, 8, 4, 2, 1]
          numbers :: [Int]
          numbers = reverse $ map digitToInt word
          computedNum :: Int
          computedNum = 11-((sum $ map (uncurry (*)) $ zip weights numbers) `mod` 11)
          numToDigit :: Int -> Char
          numToDigit 10 = '1'
          numToDigit 11 = '0'
          numToDigit x = intToDigit x
          computedDigit :: Char
          computedDigit = numToDigit computedNum

validStrings :: [String] -> Bool
validStrings = genericValidStrings localChecks globalCheck

bankIdToString :: Payer -> String
bankIdToString p = fromMaybe "" (mBankAccount p >>= Just . bankId)

officeToString :: Payer -> String
officeToString p = fromMaybe "" (mBankAccount p >>= Just . office)

controlDigitsToString :: Payer -> String
controlDigitsToString p = fromMaybe "" (mBankAccount p >>= Just . controlDigits)

numToString :: Payer -> String
numToString p = fromMaybe "" (mBankAccount p >>= Just . num)

bankAccountToString :: Payer -> String
bankAccountToString p = concatMap ($ p) [ bankIdToString
                                        , officeToString
                                        , controlDigitsToString
                                        , numToString ]

lastAndFirstName :: Payer -> String
lastAndFirstName p = (lastName p) ++ ", " ++ (firstName p)

fetchAllPayers :: Connection -> IO [Payer]
fetchAllPayers conn =
    handleSqlError $ do
      stmt <- prepare conn
                     "SELECT p.id, registration_date, \
                     \       first, last, bank_account_id \
                     \FROM   payers p, person_names n \
                     \WHERE  p.name_id = n.id;"
      execute stmt []
      results <- fetchAllRows stmt
      mapM (convertPayerRow conn) results

convertPayerRow :: Connection -> [SqlValue] -> IO Payer
convertPayerRow conn [ payerId, registrationDate
                     , first, last, bankAccountId] =
    handleSqlError $ do
      stmt <- prepare conn
                     "SELECT id, bank_id, office, control_digits, num \
                     \FROM   bank_accounts \
                     \WHERE  id = ?"
      execute stmt [bankAccountId]
      mResults <- fetchRow stmt
      return Payer
                 { payerId = ((fromSql payerId) :: Integer)
                 , registrationDate = ((fromSql registrationDate) :: Day)
                 , firstName = ((fromSql first) :: String)
                 , lastName = ((fromSql last) :: String)
                 , mBankAccount = do results <- mResults
                                     return $ convertBankAccountRow results
                 }

convertBankAccountRow :: [SqlValue] -> BankAccount
convertBankAccountRow [bankAccountId, bankId, office, controlDigits, num] =
    BankAccount { bankAccountId = ((fromSql bankAccountId) :: Integer)
                , bankId = ((fromSql bankId) :: String)
                , office = ((fromSql office) :: String)
                , controlDigits = ((fromSql controlDigits) :: String)
                , num = ((fromSql num) :: String)
                }

insertPayerFromStringsOnDb :: [String] -> Connection -> IO Payer
insertPayerFromStringsOnDb [ newFirstName
                           , newLastName
                           , newBankId
                           , newOffice
                           , newControlDigits
                           , newNum
                           ] conn =
    handleSqlError $ do
      insertNameStmt <- prepare conn 
                     "INSERT INTO person_names \
                     \(id, title, first, mid, last, suffix) \
                     \VALUES (DEFAULT, '', ?, '', ?, '')\
                     \RETURNING id"
      modifiedCount1 <- execute insertNameStmt $ map toSql [newFirstName, newLastName]
      putStrLn $ show modifiedCount1
      Just nameIdRow <- fetchRow insertNameStmt
      let nameId = (fromSql (head nameIdRow)) :: Integer
      let accountStrings = [newBankId, newOffice, newControlDigits, newNum]
          -- TODO: the following line is cut&pasted from 'update'
          accountNewEmpty = all null accountStrings
      mAccountId <- if not accountNewEmpty
         then do
           insertAccountStmt <- prepare conn
                     "INSERT INTO bank_accounts \
                     \(id, bank_id, office, control_digits, num) \
                     \VALUES (DEFAULT, ?, ?, ?, ?) \
                     \RETURNING id"
           modifiedCount2 <- execute insertAccountStmt $ map toSql accountStrings
           putStrLn $ show modifiedCount2
           Just accountIdRow <- fetchRow insertAccountStmt
           let accountId = (fromSql (head accountIdRow)) :: Integer
           return $ Just accountId
         else do
           return Nothing
      insertPayerStmt <- prepare conn
                     "INSERT INTO payers \
                     \(id, registration_date, name_id, bank_account_id) \
                     \VALUES (DEFAULT, DEFAULT, ?, ?) \
                     \RETURNING id"
      modifiedCount3 <- execute insertPayerStmt $ [toSql nameId, toSql mAccountId]
      putStrLn $ show modifiedCount3
      Just payerIdRow <- fetchRow insertPayerStmt
      queryStmt <- prepare conn
                     "SELECT p.id, registration_date, first, last, bank_account_id \
                     \FROM   payers p, person_names n \
                     \WHERE  p.name_id = n.id AND p.id = ?"
      execute queryStmt payerIdRow
      Just newPayer <- fetchRow queryStmt
      commit conn
      convertPayerRow conn newPayer

updatePayerOnDb :: [String] -> Payer -> Connection -> IO Payer
updatePayerOnDb [ newFirstName
                , newLastName
                , newBankId
                , newOffice
                , newControlDigits
                , newNum
                ] oldPayer conn =
    handleSqlError $ do
      let newPayer = oldPayer { firstName = newFirstName
                              , lastName = newLastName }
      updateNameStmt <- prepare conn 
                     "UPDATE person_names \
                     \SET first = ?, last = ? \
                     \WHERE id = (SELECT name_id FROM payers WHERE id = ?)"
      modifiedCount1 <- execute updateNameStmt [ toSql (firstName newPayer)
                                               , toSql (lastName newPayer)
                                               , toSql (payerId newPayer) ]
      putStrLn $ show modifiedCount1
      let newAccStrings = [newBankId, newOffice, newControlDigits, newNum]
          accountFuncs = [bankId, office, controlDigits, num]
          accountModified = do bankAccount <- mBankAccount oldPayer
                               let oldAccStrings = map ($ bankAccount) accountFuncs
                               return $ or (zipWith (/=) oldAccStrings newAccStrings)
          accountModifiedOrNew = fromMaybe True accountModified
          accountNewEmpty = all null [newBankId, newOffice, newControlDigits, newNum]
      returnPayer <- case (accountModifiedOrNew, accountNewEmpty) of
        (True, True) ->
            do updatePayerStmt <- prepare conn
                     "UPDATE payers \
                     \SET bank_account_id = NULL \
                     \WHERE id = ?"
               modifiedCount3 <- execute updatePayerStmt [toSql (payerId newPayer)]
               putStrLn $ show modifiedCount3
               return $ newPayer { mBankAccount = Nothing }
        (True, False) ->
            do insertAccountStmt <- prepare conn 
                     "INSERT INTO bank_accounts \
                     \(id, bank_id, office, control_digits, num) \
                     \VALUES (DEFAULT, ?, ?, ?, ?)\
                     \RETURNING id, bank_id, office, control_digits, num"
               modifiedCount2 <- execute insertAccountStmt $ map toSql newAccStrings
               putStrLn $ show modifiedCount2
               Just newAccountRow <- fetchRow insertAccountStmt
               updatePayerStmt <- prepare conn
                     "UPDATE payers \
                     \SET bank_account_id = ? \
                     \WHERE id = ?"
               modifiedCount3 <- execute updatePayerStmt [ head newAccountRow
                                                         , toSql (payerId newPayer) ]
               putStrLn $ show modifiedCount3
               return $ newPayer { mBankAccount 
                                       = Just $ convertBankAccountRow newAccountRow }
        (False, _) -> 
            do putStrLn "Bank account not modified."
               return newPayer
      commit conn
      return returnPayer

deletePayerOnDb :: Payer -> Connection -> IO ()
deletePayerOnDb p conn = 
    handleSqlError $ do
      -- WARNING: bank accounts are not deleted
      deletePayerStmt <- prepare conn 
                     "DELETE FROM payers WHERE id = ? RETURNING name_id"
      modifiedCount1 <- execute deletePayerStmt [toSql (payerId p)]
      putStrLn $ show modifiedCount1
      Just deletedPayerNameId <- fetchRow deletePayerStmt
      deleteNameStmt <- prepare conn
                     "DELETE FROM person_names \
                     \WHERE id = ?"
      modifiedCount2 <- execute deleteNameStmt deletedPayerNameId
      putStrLn $ show modifiedCount2
      commit conn
