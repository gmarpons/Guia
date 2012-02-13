module Guia.PayersPanel where

import Data.IORef
    (IORef, newIORef, readIORef, writeIORef)
import Graphics.UI.Gtk
    (listStoreAppend)

import Guia.AppData
import Guia.Utils
import Guia.Gui.GuiUtils
import Guia.Gui.PayersGladeGui
import Guia.Db.Payer

panelId :: PanelId
panelId = "payers"

connectPayers :: IORef AppData -> IO ()
connectPayers adRef = do
  ad <- readIORef adRef
  let b = builder ad
      conn = connection ad
  gui <- mkPayersGui b

  payersMd <- mkTreeViewModel (payersTv gui) Nothing [ lastName
                                                     , firstName
                                                     , show . registrationDate]
  writeIORef adRef (mainState ad, mainGui ad, builder ad, connection ad, 
                    Just payersMd, billingConceptsLs ad)
  allPayers <- fetchAllPayers conn
  mapM_ (listStoreAppend payersMd) allPayers

  stRef <- (newIORef NoSel) :: IO (IORef State)

  connectItemPanel adRef gui stRef panelId validStrings payersMd
