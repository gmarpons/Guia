module Guia.DebitsFile
    (writeInvoicingToDebitsFile)
where

import Data.DList as DL
    (DList(..), empty, fromList, toList, append, replicate, concat)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Time.Format
    (formatTime)
import System.Locale
    (defaultTimeLocale)
import Data.Char
    (toUpper, isAlphaNum)
import Data.Maybe
    (Maybe(..), fromMaybe)
import System.IO
    (openBinaryFile, IOMode(WriteMode), hPutStr, hClose)
import System.Environment
    (getEnv)
import Data.Encoding
    (encodeString)
import Data.Encoding.CP850
    (CP850(..))
import Numeric
    (showFFloat)
import Data.List
    (sortBy, groupBy, splitAt)
import Data.Ord
    (comparing)
import Data.Time.Calendar
    (Day, toGregorian)
import Data.Time.Clock
    (getCurrentTime, utctDay)
import System.Locale
    (defaultTimeLocale, TimeLocale(..))
import qualified Data.HashTable as HT
    (HashTable(..), fromList, lookup)

import Guia.Db.Invoicing
import Guia.Db.Applicant as APP
import Guia.Db.Payer

-- TODO: see what happens with Registro Sexto Opcional (with address info)

writeInvoicingToDebitsFile :: Applicant -> Invoicing -> [Charge] -> [Payer] -> IO ()
writeInvoicingToDebitsFile app inv charges payers = do
  putStrLn $ "Applicant: " ++ show app
  homePath <- getEnv "HOME"
  let -- TODO: allow user to change this dir, create it, etc.
      debitsDir = "FitxersCaixa"
      fileName = filter isAlphaNum $ invoicingDescription inv
      fileExtension = "." ++ "dat"
      fileFullPath = homePath ++ "/" ++ debitsDir ++ "/" ++ fileName ++ fileExtension
  fileHandle <- openBinaryFile fileFullPath WriteMode
  let payersWithKey = zip (map payerId payers) payers
  payersHashTable <- HT.fromList fromIntegral payersWithKey
  ((_, wr), _) 
      <- runRegisterBuilder (allInvoicingRegisters charges) app inv payersHashTable
  hPutStr fileHandle $ encodeString CP850 (toList wr)
  hClose fileHandle

type RegisterBuilder = ReaderT Rd (WriterT Wr (StateT St IO))
data Rd = Rd
    { applicant :: Applicant
    , invoicing :: Invoicing
    , payers :: HT.HashTable Integer Payer
    }
type Wr = DList Char
data St = St
    { numRegisters :: Int
    , numInvoices :: Int
    , totalAmount :: Int
    , invoicePayer :: Maybe Payer
    , invoiceAmount :: Int
    , invoiceId :: Integer
    , invoiceConceptStrings :: [DList Char]
    , numOptionalRegsGenerated :: Int
    }

runRegisterBuilder :: RegisterBuilder a 
                   -> Applicant 
                   -> Invoicing 
                   -> HT.HashTable Integer Payer
                   -> IO ((a, Wr), St)
runRegisterBuilder regBuilder app inv payers =
    let rd = Rd { applicant = app, invoicing = inv, payers = payers }
        st = St { numRegisters = 0, numInvoices = 0, totalAmount = 0
                , invoicePayer = Nothing, invoiceAmount = 0
                , invoiceId = 0, invoiceConceptStrings = [] 
                , numOptionalRegsGenerated = 0 }
    in runStateT (runWriterT (runReaderT regBuilder rd)) st

type Invoice = (Payer, [Charge])

allInvoicingRegisters :: [Charge] -> RegisterBuilder ()
allInvoicingRegisters chargesUnsorted = do
  payers <- asks payers
  let chargePayerIdOrd = comparing chargePayerId
      chargesSorted = sortBy chargePayerIdOrd chargesUnsorted
      equalPayer x y = chargePayerId x == chargePayerId y
      chargesGroupedByPayer = groupBy equalPayer chargesSorted
      getInvoice charges@(fstCharge : _) = 
          do Just payer <- liftIO $ HT.lookup payers (chargePayerId fstCharge)
             return (payer, charges)
  invoices <- mapM getInvoice chargesGroupedByPayer
  let invoiceToAccountAndPayerId (p, _) = (bankAccountToString p, payerId p)
      invoiceAccountOrd = comparing invoiceToAccountAndPayerId
      invoicesSorted = sortBy invoiceAccountOrd invoices
  build RegistroCabeceraPresentador
  build RegistroCabeceraOrdenante
  mapM_ buildInvoiceRegisters invoicesSorted
  build RegistroTotalOrdenante
  build RegistroTotalGeneral
  return ()

-- 'groupBy' guarantees that the list of charges is not null
buildInvoiceRegisters :: Invoice -> RegisterBuilder ()
buildInvoiceRegisters (payer, charges@(fstCharge:_)) = do
  st <- get
  -- payers <- asks payers
  -- Just payer <- liftIO $ HT.lookup payers (chargePayerId fstCharge)
  let (totalBase, totalVat) = getInvoiceAmountsAsInt charges
      currentInvoiceAmount = totalBase + totalVat
      vatStrings = [ padDListR 40 ' ' $ fromList "Iva"
                   , padDListL 40 ' ' $ fromList $ to2DecimalsString totalVat ]
  put st { numInvoices = (numInvoices st) + 1
         , totalAmount = (totalAmount st) + currentInvoiceAmount
         , invoicePayer = Just payer
         , invoiceAmount = currentInvoiceAmount
         , invoiceId = chargeInvoiceId fstCharge
         , invoiceConceptStrings = 
             (concatMap getConceptStrings charges) ++ vatStrings
         , numOptionalRegsGenerated = 0 }
  build RegistroIndividualObligatorio
  buildOptionalRegisters
  return ()

buildOptionalRegisters :: RegisterBuilder ()
buildOptionalRegisters = do
  st <- get
  case invoiceConceptStrings st of
    [] -> return ()
    _  -> do build RegistroIndividualOpcional
             buildOptionalRegisters

getConceptStrings :: Charge -> [DList Char]
getConceptStrings c =
    let (fstString, sndString) = splitAt 40 (actualDescription c)
        price = fromList $ to2DecimalsString $ toRoundedIntx100 $ actualBasePrice c
    in [ padDListR 40 ' ' $ fromList fstString
       , DL.append (padDListL 30 ' ' $ fromList sndString) (padDListL 10 ' ' price) ]

getInvoiceAmountsAsInt :: [Charge] -> (Int, Int)
getInvoiceAmountsAsInt charges =
    let chargeBaseAsInt :: Charge -> Int
        chargeBaseAsInt = toRoundedIntx100 . actualBasePrice
    in ( foldr ((+) . toRoundedIntx100 . actualBasePrice) 0 charges
       , foldr ((+) . toRoundedIntx100 . chargeVatAmount) 0 charges )

toRoundedIntx100 :: Double -> Int
toRoundedIntx100 = round . (* 100)

to2DecimalsString :: Int -> String
to2DecimalsString i =
    let iAsString = "  " ++ show i
        (initOfI, endOfI) = splitAt (length iAsString - 2) iAsString
    in initOfI ++ (',' : endOfI)

class RegistroCuaderno19Class regT where
    a             :: regT -> RegisterBuilder regT
    a              = ret $ return ()
    a1            :: regT -> RegisterBuilder regT
    a1             = ret $ return ()
    a2            :: regT -> RegisterBuilder regT
    a2             = ret $ return ()
    b             :: regT -> RegisterBuilder regT
    b              = ret $ return ()
    b1            :: regT -> RegisterBuilder regT
    b1             = ret $ do app <- asks applicant
                              tell $ fromList (nif app)
                              tell $ fromList (suffix app)
    b2            :: regT -> RegisterBuilder regT
    b2             = ret $ return ()
    b3            :: regT -> RegisterBuilder regT
    b3             = ret $ return ()
    c             :: regT -> RegisterBuilder regT
    c              = ret $ return ()
    d             :: regT -> RegisterBuilder regT
    d              = ret $ return ()
    d1            :: regT -> RegisterBuilder regT
    d1             = ret $ return ()
    d2            :: regT -> RegisterBuilder regT
    d2             = ret $ return ()
    d3            :: regT -> RegisterBuilder regT
    d3             = ret $ return ()
    d4            :: regT -> RegisterBuilder regT
    d4             = ret $ return ()
    e             :: regT -> RegisterBuilder regT
    e              = ret $ return ()
    e1            :: regT -> RegisterBuilder regT
    e1             = ret $ return ()
    e2            :: regT -> RegisterBuilder regT
    e2             = ret $ return ()
    e3            :: regT -> RegisterBuilder regT
    e3             = ret $ return ()
    f             :: regT -> RegisterBuilder regT
    f              = ret $ return ()
    f1            :: regT -> RegisterBuilder regT
    f1             = ret $ return ()
    f2            :: regT -> RegisterBuilder regT
    f2             = ret $ return ()
    f3            :: regT -> RegisterBuilder regT
    f3             = ret $ return ()
    g             :: regT -> RegisterBuilder regT
    g              = ret $ return ()
    h             :: regT -> RegisterBuilder regT
    h              = ret $ return ()
    build         :: regT -> RegisterBuilder regT
    build         =       a >=> a1 >=> a2
                      >=> b >=> b1 >=> b2 >=> b3
                      >=> c
                      >=> d >=> d1 >=> d2 >=> d3 >=> d4
                      >=> e >=> e1 >=> e2 >=> e3
                      >=> f >=> f1 >=> f2 >=> f3
                      >=> g
                      >=> h 
                      >=> endOfReg
    endOfReg      :: regT -> RegisterBuilder regT
    endOfReg       = ret $ do tell $ fromList "\r\n"
                              st <- get
                              let n = numRegisters st
                              put st { numRegisters = n + 1 }

data RegistroCabeceraPresentador = RegistroCabeceraPresentador
instance RegistroCuaderno19Class RegistroCabeceraPresentador where
    a1 = ret $ tell $ fromList "51" -- Euros
    a2 = ret $ tell $ fromList "80"
    b2 = ret $ do inv <- asks invoicing
                  tell $ issuingDateAsDString inv
    b3 = ret $ tell $ DL.replicate 6 ' '
    c  = ret $ do app <- asks applicant
                  tell $ fullNameAsDString app
    d  = ret $ tell $ DL.replicate 20 ' '
    e1 = ret $ do app <- asks applicant
                  tell $ bankAccountItemToDString bankId (APP.mBankAccount app)
    e2 = ret $ do app <- asks applicant
                  tell $ bankAccountItemToDString office (APP.mBankAccount app)
    e3 = ret $ tell $ DL.replicate 12 ' '
    f  = ret $ tell $ DL.replicate 40 ' '
    g  = ret $ tell $ DL.replicate 14 ' '

data RegistroCabeceraOrdenante = RegistroCabeceraOrdenante
instance RegistroCuaderno19Class RegistroCabeceraOrdenante where
    a1 = ret $ tell $ fromList "53"
    a2 = ret $ tell $ fromList "80"
    b2 = ret $ do inv <- asks invoicing
                  tell $ issuingDateAsDString inv
    -- TODO: b3 should be today.
    b3 = b2
    c  = ret $ do app <- asks applicant
                  tell $ fullNameAsDString app
    d1 = ret $ do app <- asks applicant
                  tell $ bankAccountItemToDString bankId (APP.mBankAccount app)
    d2 = ret $ do app <- asks applicant
                  tell $ bankAccountItemToDString office (APP.mBankAccount app)
    d3 = ret $ do app <- asks applicant
                  tell $ bankAccountItemToDString controlDigits (APP.mBankAccount app)
    d4 = ret $ do app <- asks applicant
                  tell $ bankAccountItemToDString num (APP.mBankAccount app)
    e1 = ret $ tell $ DL.replicate 8 ' '
    e2 = ret $ tell $ fromList "01"
    e3 = ret $ tell $ DL.replicate 10 ' '
    f  = ret $ tell $ DL.replicate 40 ' '
    g  = ret $ tell $ DL.replicate 14 ' '

data RegistroTotalOrdenante = RegistroTotalOrdenante
instance RegistroCuaderno19Class RegistroTotalOrdenante where
    a1 = ret $ tell $ fromList "58"
    a2 = ret $ tell $ fromList "80"
    b2 = ret $ tell $ DL.replicate 12 ' '
    c  = ret $ tell $ DL.replicate 40 ' '
    d  = ret $ tell $ DL.replicate 20 ' '
    e1 = ret $ do st <- get
                  tell $ padDListL 10 '0' $ fromList $ show $ totalAmount st
    e2 = ret $ tell $ DL.replicate 6 ' '
    f1 = ret $ do st <- get
                  tell $ padDListL 10 '0' $ fromList $ show $ numInvoices st
    f2 = ret $ do st <- get
                  tell $ padDListL 10 '0' $ fromList $ show $ numRegisters st
    f3 = ret $ tell $ DL.replicate 20 ' '
    g  = ret $ tell $ DL.replicate 18 ' '

data RegistroTotalGeneral = RegistroTotalGeneral
instance RegistroCuaderno19Class RegistroTotalGeneral where
    a1 = ret $ tell $ fromList "59"
    a2 = ret $ tell $ fromList "80"
    b2 = ret $ tell $ DL.replicate 12 ' '
    c  = ret $ tell $ DL.replicate 40 ' '
    d1 = ret $ tell $ fromList "0001"
    d2 = ret $ tell $ DL.replicate 16 ' '
    e1 = ret $ do st <- get
                  tell $ padDListL 10 '0' $ fromList $ show $ totalAmount st
    e2 = ret $ tell $ DL.replicate 6 ' '
    f1 = ret $ do st <- get
                  tell $ padDListL 10 '0' $ fromList $ show $ numInvoices st
    f2 = ret $ do st <- get
                  tell $ padDListL 10 '0' $ fromList $ show $ (numRegisters st + 1)
    f3 = ret $ tell $ DL.replicate 20 ' '
    g  = ret $ tell $ DL.replicate 18 ' '

class RegistroCuaderno19Class regT => RegistroIndividualClass regT where
    a1' :: regT -> RegisterBuilder regT
    a1'  = ret $ tell $ fromList "56"
    b2' :: regT -> RegisterBuilder regT
    b2'  = ret $ do st <- get
                    let Just payer = invoicePayer st
                    tell $ padDListL 12 '0' $ fromList $ show $ payerId payer

data RegistroIndividualObligatorio = RegistroIndividualObligatorio
instance RegistroIndividualClass RegistroIndividualObligatorio
instance RegistroCuaderno19Class RegistroIndividualObligatorio where
    a1 = a1'
    b2 = b2'
    a2 = ret $ tell $ fromList "80"
    c  = ret $ do st <- get
                  let Just payer = invoicePayer st
                  tell $ padDListR 40 ' ' $ fromList $ lastAndFirstName payer
    d1 = ret $ do st <- get
                  let Just payer = invoicePayer st
                  tell $ fromList $ bankIdToString payer
    d2 = ret $ do st <- get
                  let Just payer = invoicePayer st
                  tell $ fromList $ officeToString payer
    d3 = ret $ do st <- get
                  let Just payer = invoicePayer st
                  tell $ fromList $ controlDigitsToString payer
    d4 = ret $ do st <- get
                  let Just payer = invoicePayer st
                  tell $ fromList $ numToString payer
    e  = ret $ do st <- get
                  let amountToCharge = invoiceAmount st
                  tell $ padDListL 10 '0' $ fromList $ show amountToCharge
    f1 = ret $ tell $ DL.replicate 6 '0'
    f2 = ret $ do st <- get
                  let currentInvoiceId = invoiceId st
                  tell $ padDListL 10 '0' $ fromList $ show currentInvoiceId
    -- At least one concept (plus Vat line) is guaranteed
    g  = ret $ processConceptString
    h  = ret $ tell $ DL.replicate 8 ' '

data RegistroIndividualOpcional = RegistroIndividualOpcional
instance RegistroIndividualClass RegistroIndividualOpcional
instance RegistroCuaderno19Class RegistroIndividualOpcional where
    a1 = a1'
    b2 = b2'
    a2 = ret $ do st <- get
                  let nextOptionalRegister = (numOptionalRegsGenerated st) + 1
                  put st { numOptionalRegsGenerated = nextOptionalRegister }
                  tell $ fromList $ show $ nextOptionalRegister + 80
    c  = ret $ processConceptString
    d  = ret $ processConceptString
    e  = ret $ processConceptString
    f  = ret $ tell $ DL.replicate 14 ' '

processConceptString :: RegisterBuilder ()
processConceptString = do 
  st <- get
  case invoiceConceptStrings st of
    [] -> tell $ padDListR 40 ' ' empty
    (nextConceptString : conceptStrings) -> do
                                  put st { invoiceConceptStrings = conceptStrings }
                                  tell $ nextConceptString

ret :: (Monad m) => m () -> a -> m a
ret f v = do
  f
  return v

padDListL :: Int -> a -> DList a -> DList a
padDListL padding elem dlist = fromList longerList
    where shorterList = take padding (toList dlist)
          longerList = reverse $ take padding (reverse shorterList ++ repeat elem)

padDListR :: Int -> a -> DList a -> DList a
padDListR padding elem dlist = fromList longerList
    where shorterList = take padding (toList dlist)
          longerList = take padding (shorterList ++ repeat elem)

bankAccountItemToDString :: (BankAccount -> String)
                         -> Maybe BankAccount
                         -> DList Char
bankAccountItemToDString item mBA =
    fromMaybe (DL.replicate 4 ' ') (mBA >>= Just . fromList . item)

issuingDateAsDString :: Invoicing -> DList Char
issuingDateAsDString inv = fromList (formatTime defaultTimeLocale "%d%m%y" date)
    where date = issuingDate inv

fullNameAsDString :: Applicant -> DList Char
fullNameAsDString app = padDListR 40 ' ' nameInDString 
    where nameStrings = [appFirstName app, " ", appLastName app]
          upperNameStrings = map (map toUpper) nameStrings
          nameInDString = DL.concat $ map fromList upperNameStrings
