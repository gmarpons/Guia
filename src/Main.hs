module Main where

import Graphics.UI.Gtk 
    (initGUI, on, deleteEvent, buttonActivated, mainQuit, widgetShowAll, 
     mainGUI, widgetDestroy, on, buttonActivated, toggleButtonGetActive)
import Control.Monad.Trans 
    (liftIO)
import Control.Monad
    (unless)
import Data.IORef 
    (IORef, newIORef, readIORef, writeIORef)
import Control.Concurrent 
    (threadDelay)

import Guia.Db.Database
import Guia.Gui.MainGladeGui
import Guia.AppData
import Guia.PayersPanel
import Guia.BillingConceptsPanel
import Guia.InvoicingsPanel

connectPanelFuncs :: [IORef AppData -> IO ()]
connectPanelFuncs =  [connectPayers, connectBillingConcepts, connectInvoicings]

-- TODO: avoid more than 7 charges in an invoice
-- TODO: check all DB constraints and write code to apply them in haskell
-- TODO: avoid the user can select payer without a bank account for an invoicing
-- TODO: new column in payers view for payerId
-- TODO: new columns in charges view: price with VAT
-- TODO: fast search field for payers
-- TODO: show info dialog if db cannot be connected to
-- TODO: show info dialog if debits file cannot be open/written to
-- TODO: look for settings file at startup (create if doesn't exist) for paths, etc.
-- TODO: put fields for actual account in charges
-- TODO: combobox invoices put: "Selecciona la facturació a clonar o editar"
-- TODO: when some item is added to a treemodel, select and show the item in the view
-- TODO: In totals of invoicings view, don't show total without VAT
-- TODO: when cloning, don't copy "Matrícules"
-- TODO: choose a good naming for invoicings and debits file names:
--       - not in English
--       - with date first, for correct file sorting
-- TODO: in view, show (and read) decimals with ",".
-- TODO: show icons in any computer
-- TODO: add empty invoicing to clone when needed.
-- TODO: Indicate in some way that a CCC is not correct (e.g. digits in red)
-- TODO: Don't replicate concepts such as annual fee.
-- TODO: Enhance invoicings panel with some text that helps understand workflow.
-- TODO: Add totals to printed listing.
-- TODO: Translate headers in printed listing.

main :: IO ()
main = do
  isRunning <- isServerRunning
  unless isRunning startServer
  let wasRunning = isRunning
  initGUI
  mConn <- connectRec delays
  case mConn of
    Nothing -> return ()
    Just conn -> do
               builder <- getGuiBuilder gladepath
               gui <- mkMainGui builder
               let panelId = head (panelIds gui)
               adRef <- newIORef (View panelId, gui, builder, conn, Nothing, Nothing)
               mapM_ (\f -> f adRef) (connectMainGui:connectPanelFuncs)
               widgetShowAll (mainWd gui)
               mainGUI
               disconnect conn
  unless wasRunning stopServer
    where
      -- Application is expected to run in src/.
      -- TODO Make it possible to run app in another dir.
      gladepath = "../glade/guia.glade"
      initialDelay = 200000
      -- We wait at most 10 seconds
      delays = takeWhile (< 10000001) [(2 ^ n) * initialDelay | n <- [0..]]
      connectRec :: [Int] -> IO (Maybe Connection)
      connectRec [] = return Nothing
      connectRec (delay:nextDelays) = do
         mConn <- connect
         case mConn of
           Just conn -> return $ Just conn
           Nothing -> do threadDelay delay
                         connectRec nextDelays

connectMainGui :: IORef AppData -> IO ()
connectMainGui adRef = do
  ad <- readIORef adRef
  let gui = mainGui ad
  on (mainWd gui) deleteEvent $ liftIO mainQuit >> return False
  on (mwExitBt gui) buttonActivated $ widgetDestroy (mainWd gui) >> mainQuit
  mapM_ (\chs -> on chs buttonActivated $ do -- Signal 'toggled' can also be used
                   isActive <- toggleButtonGetActive chs
                   if isActive
                      then do let newPanelId = getPanelIdFromChooser gui chs
                              setMainState (View newPanelId) adRef
                      else return ()
        ) $ panelChoosers gui
