module Guia.Db.Applicant
    (Applicant(..), fetchApplicant)
where

import Database.HDBC
import Database.HDBC.PostgreSQL
    (Connection)

import Guia.Db.Payer
    (BankAccount(..), convertBankAccountRow)

data Applicant = Applicant
    { applicantId :: Integer
    , suffix :: String
    , nif :: String
    , appFirstName :: String
    , appLastName :: String
    , mBankAccount :: Maybe BankAccount
    } deriving (Show)

fetchApplicant :: Connection -> IO Applicant
fetchApplicant conn =
    handleSqlError $ do
      stmt <- prepare conn
                     "SELECT a.id, a.suffix, nif, first, last, bank_account_id \
                     \FROM   applicant a, person_names n \
                     \WHERE  a.name_id = n.id;"
      execute stmt []
      Just appRow <- fetchRow stmt
      convertApplicantRow conn appRow

convertApplicantRow :: Connection -> [SqlValue] -> IO Applicant
convertApplicantRow conn [applicantId, suffix, nif, first, last, bankAccountId] =
    handleSqlError $ do
      stmt <- prepare conn
                     "SELECT id, bank_id, office, control_digits, num \
                     \FROM   bank_accounts \
                     \WHERE  id = ?"
      execute stmt [bankAccountId]
      mResults <- fetchRow stmt
      return Applicant
                 { applicantId = ((fromSql applicantId) :: Integer)
                 , suffix = toStringOfLengthThree ((fromSql suffix) :: Integer)
                 , nif = ((fromSql nif) :: String)
                 , appFirstName = ((fromSql first) :: String)
                 , appLastName = ((fromSql last) :: String)
                 , mBankAccount = do results <- mResults
                                     return $ convertBankAccountRow results
                 }

toStringOfLengthThree :: Integer -> String
toStringOfLengthThree = reverse . take 3 . reverse . ("000" ++) . show