module Guia.InvoicingsPanel where

import Data.IORef
    (IORef, newIORef, readIORef, writeIORef)
import Graphics.UI.Gtk
    -- (on, toggled, toggleButtonGetActive)
import Numeric
    (showFFloat)
import Data.List
    (nub)
import System.Process
    (runCommand, waitForProcess)
import System.IO
    (openBinaryTempFile)
import System.Directory
    (removeFile)

import Guia.AppData
import Guia.Utils
import Guia.DebitsFile
import Guia.Gui.InvoicingsGladeGui
import Guia.Db.Payer hiding (validStrings)
import Guia.Db.BillingConcept hiding (validStrings)
import Guia.Db.Invoicing
import Guia.Db.Applicant
import Guia.Db.DataUtils

panelId :: PanelId
panelId = "invoicings"

connectInvoicings :: IORef AppData -> IO ()
connectInvoicings adRef = do
  ad <- readIORef adRef
  let b = builder ad
      conn = connection ad
  gui <- mkInvoicingsGui b

  -- WARNING! Next lines silently make new (ListStore a) if one doesn't exist!
  payersInInvoicingSt 
      <- mkTreeViewModel (payersInInvoicingTv gui) (payersLs ad) [lastName, firstName]
  billingConceptsInInvoicingSt 
      <- mkTreeViewModel (billingConceptsInInvoicingTv gui) (billingConceptsLs ad) 
         [description, basePriceToString, finalPriceToString]

  invoicingsSt <- mkComboBoxModel (invoicingsCb gui) invoicingDescription

  allInvoicings <- fetchAllInvoicings conn
  mapM_ (listStoreAppend invoicingsSt) allInvoicings

  stRef <- (newIORef NoSel) :: IO (IORef State)

  connectItemPanel adRef gui stRef panelId validStrings invoicingsSt

  chargesMd 
      <- mkTreeViewModel (chargesTv gui) Nothing 
         [ chargeLastName
         , actualDescription
         , actualBasePriceToString
         ]

  let computeAndShowTotals = do 
       charges <- listStoreToList chargesMd
       let -- TODO: repeated in DebitsFile.hs
           toRoundedIntx100 :: Double -> Int
           toRoundedIntx100 = round . (* 100)
           basePrices = map actualBasePrice charges
           -- vatRatios = map actualVatRatio charges
           vats = map chargeVatAmount charges
           invoices = map chargeInvoiceId charges
           totalBasePrice = sum basePrices
           basePricesAsInts = map toRoundedIntx100 basePrices
           vatsAsInts = map toRoundedIntx100 vats
           -- vats = zipWith (*) basePricesAsInts vatRatiosAsInts
           -- totalFinalPrice = totalBasePrice + (sum $ zipWith (*) basePrices vats)
           totalFinalPriceAsInt = sum basePricesAsInts + sum vatsAsInts
           totalFinalPrice = (fromIntegral totalFinalPriceAsInt) / 100
           dec = Just 2
       set (invoicingBasePriceEn gui) [entryText := showFFloat dec totalBasePrice ""]
       set (invoicingFinalPriceEn gui) [entryText:=showFFloat dec totalFinalPrice ""]
       set (invoicingNumberOfInvoicesEn gui) [entryText:=show $ length (nub invoices)]

  set (addChargeBt gui) [widgetSensitive := False]
  set (chargeActualBasePriceEn gui) [widgetSensitive := False]
  set (deleteChargeBt gui) [widgetSensitive := False]

  on (invoicingsCb gui) changed $ do
    mIt <- comboBoxGetActiveIter (invoicingsCb gui)
    putStrLn $ "Selected invoicing iter: " ++ show mIt
    listStoreClear chargesMd
    case mIt of
      (Just it) -> do
               putStrLn "Seleccio cambiada"
               row <- treeModelGetRow invoicingsSt it
               charges <- fetchChargesOfAnInvoicing row conn
               mapM_ (listStoreAppend chargesMd) charges
      Nothing -> do
               putStrLn "No invoicing selected 2"
    computeAndShowTotals

  on (cloneInvoicingBt gui) buttonActivated $ do
    (description, today) <- invoicingNewDescription conn
    set (invoicingDescriptionEn gui) [entryText := description]
    set (invoicingEditionDateEn gui) [entryText := show today]
    charges <- listStoreToList chargesMd
    invoicing <- cloneInvoicingOnDb charges conn
    listStoreInsert invoicingsSt 0 invoicing
    comboBoxSetActive (invoicingsCb gui) 0
    putStrLn "Clone done."

  on (printInvoicingBt gui) buttonActivated $ do
    (Just it) <- comboBoxGetActiveIter (invoicingsCb gui)
    putStrLn "Generating PDF file."
    applicant <- fetchApplicant conn
    invoicing <- treeModelGetRow invoicingsSt it
    charges <- listStoreToList chargesMd
    printInvoiceToPdfAndShow invoicing
    let Just payersLs' = payersLs ad
    payers <- listStoreToList payersLs'
    writeInvoicingToDebitsFile applicant invoicing charges payers

  payersSel <- treeViewGetSelection (payersInInvoicingTv gui)
  billingConceptsSel <- treeViewGetSelection (billingConceptsInInvoicingTv gui)
  let onSelChangedAction = do
       countPayersSel <- treeSelectionCountSelectedRows payersSel
       countBillingConceptsSel <- treeSelectionCountSelectedRows billingConceptsSel
       state <- readIORef stRef
       case (countPayersSel > 0, countBillingConceptsSel > 0, state) of
         (True, True, EditNew _ _) -> do
                             set (addChargeBt gui) [widgetSensitive := True]
                             set (chargeActualBasePriceEn gui) [widgetSensitive:=True]
         (True, True, EditOld _ _) -> do
                             set (addChargeBt gui) [widgetSensitive := True]
                             set (chargeActualBasePriceEn gui) [widgetSensitive:=True]
         _                         -> do
                             set (addChargeBt gui) [widgetSensitive := False]
                             set (chargeActualBasePriceEn gui)[widgetSensitive:=False]
  on payersSel treeSelectionSelectionChanged onSelChangedAction
  on billingConceptsSel treeSelectionSelectionChanged onSelChangedAction

  chargesSel <- treeViewGetSelection (chargesTv gui)
  on chargesSel treeSelectionSelectionChanged $ do
       countChargesSel <- treeSelectionCountSelectedRows chargesSel
       state <- readIORef stRef
       case (countChargesSel > 0, state) of
         (True, EditNew _ _) -> set (deleteChargeBt gui) [widgetSensitive:=True]
         (True, EditOld _ _) -> set (deleteChargeBt gui) [widgetSensitive:=True]
         _                   -> set (deleteChargeBt gui) [widgetSensitive:=False]

  on (editInvoicingTb gui) toggled $ do 
    countPayersSel <- treeSelectionCountSelectedRows payersSel
    countBillingConceptsSel <- treeSelectionCountSelectedRows billingConceptsSel
    countChargesSel <- treeSelectionCountSelectedRows chargesSel
    isActive <- toggleButtonGetActive (editInvoicingTb gui)
    case (countPayersSel > 0, countBillingConceptsSel > 0, isActive) of
      (True, True, True) -> do
                            set (addChargeBt gui) [widgetSensitive := True]
                            set (chargeActualBasePriceEn gui) [widgetSensitive:=True]
      _                  -> do
                            set (addChargeBt gui) [widgetSensitive := False]
                            set (chargeActualBasePriceEn gui)[widgetSensitive:=False]
    case (countChargesSel > 0, isActive) of
      (True, True) -> set (deleteChargeBt gui) [widgetSensitive:=True]
      _            -> set (deleteChargeBt gui) [widgetSensitive:=False]

  on (chargeActualBasePriceEn gui) editableChanged $ do
    text <- get (chargeActualBasePriceEn gui) entryText
    state <- readIORef stRef
    case (null text || isNumeric text, state) of
      (True, EditNew _ _) -> set (addChargeBt gui) [widgetSensitive:=True]
      (True, EditOld _ _) -> set (addChargeBt gui) [widgetSensitive:=True]
      _                   -> set (addChargeBt gui) [widgetSensitive:=False]

  on (cancelInvoicingBt gui) buttonActivated $ do
    set (chargeActualBasePriceEn gui) [entryText := ""]

  on (saveInvoicingBt gui) buttonActivated $ do
    set (chargeActualBasePriceEn gui) [entryText := ""]
    computeAndShowTotals

  on (addChargeBt gui) buttonActivated $ do
    -- TODO: code can be probably simplified by use of treeModelGetRow
    payersSm <- treeViewGetSortedModel (payersInInvoicingTv gui)
    Just iter <- treeSelectionGetSelected payersSel
    childIter <- treeModelSortConvertIterToChildIter payersSm iter
    let index = listStoreIterToIndex childIter
    payer <- listStoreGetValue payersInInvoicingSt index
    billingConceptsSm <- treeViewGetSortedModel (billingConceptsInInvoicingTv gui)
    Just iter <- treeSelectionGetSelected billingConceptsSel
    childIter <- treeModelSortConvertIterToChildIter billingConceptsSm iter
    let index = listStoreIterToIndex childIter
    billingConcept <- listStoreGetValue billingConceptsInInvoicingSt index
    Just invoicingIter <- comboBoxGetActiveIter (invoicingsCb gui)
    invoicing <- treeModelGetRow (invoicingsSt) invoicingIter
    actualBasePriceString <- get (chargeActualBasePriceEn gui) entryText
    let actualBasePrice = if null actualBasePriceString then basePrice billingConcept
                          else (read actualBasePriceString) :: Double
    charge <- insertChargeOnDb (invoicingId invoicing) (payerId payer)
                          (description billingConcept) actualBasePrice 
                          (vatRatio billingConcept) conn
    listStoreAppend chargesMd charge
    set (chargeActualBasePriceEn gui) [entryText := ""]
    putStrLn "Added charge."
    computeAndShowTotals

  on (deleteChargeBt gui) buttonActivated $ do
    chargesSm <- treeViewGetSortedModel (chargesTv gui)
    treeSelectionSelectedForeach chargesSel $ \iter -> do
      childIter <- treeModelSortConvertIterToChildIter chargesSm iter
      let index = listStoreIterToIndex childIter
      charge <- listStoreGetValue chargesMd index
      deleteChargeOnDb charge conn
      listStoreRemove chargesMd index
    putStrLn "Deleted charge."
    computeAndShowTotals

  return ()

printInvoiceToPdfAndShow :: Invoicing -> IO ()
printInvoiceToPdfAndShow inv = do
  -- TODO: use $TEMP dir instead of /tmp
  (filename, fileHandle) <- openBinaryTempFile "/tmp" "guia.pdf"
  let genCmd = "psql -p 6543 -h /tmp -U guia guia --command \"\
               \SELECT n.last, n.first, c.actual_description, \
               \c.actual_base_price \
               \FROM payers p, person_names n, charges c, invoices i \
               \WHERE c.invoice_id_is_part_of = i.id \
               \AND i.invoicing_id_is_included_in = " ++ (show $ invoicingId inv) ++
               "AND p.name_id = n.id AND i.payer_id_is_charged_to = p.id \
               \AND p.id >= 000 \
               \ORDER BY n.last\"\
               \| paps --font=\"Monospace 9\" | ps2pdf - " ++ filename
      -- TODO: allow changing visualizer in config file (default can be evince)
      viewCmd = "evince " ++ filename
  genHandle <- runCommand genCmd
  waitForProcess genHandle
  runCommand viewCmd
  -- removeFile filename
  return ()
