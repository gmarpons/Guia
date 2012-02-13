module Guia.BillingConceptsPanel where

import Data.IORef
    (IORef, newIORef, readIORef, writeIORef)
import Graphics.UI.Gtk
    (listStoreAppend)

import Guia.AppData
import Guia.Utils
import Guia.Gui.GuiUtils
import Guia.Gui.BillingConceptsGladeGui
import Guia.Db.BillingConcept

panelId :: PanelId
panelId = "billingConcepts"

connectBillingConcepts :: IORef AppData -> IO ()
connectBillingConcepts adRef = do
  ad <- readIORef adRef
  let b = builder ad
      conn = connection ad
  gui <- mkBillingConceptsGui b

  billingConceptsLs 
      <- mkTreeViewModel (billingConceptsTv gui) Nothing [ description
                                                         , basePriceToString
                                                         , vatRatioToString
                                                         , finalPriceToString ]
  writeIORef adRef (mainState ad, mainGui ad, builder ad, connection ad, 
                    payersLs ad, Just billingConceptsLs)
  allBillingConcepts <- fetchAllBillingConcepts conn
  mapM_ (listStoreAppend billingConceptsLs) allBillingConcepts

  stRef <- (newIORef NoSel) :: IO (IORef State)

  connectItemPanel adRef gui stRef panelId validStrings billingConceptsLs
