module Guia.AppData
    (MainState(..), PanelId, AppData, mainState, mainGui, builder, connection,
     payersLs, billingConceptsLs, setMainState)
where

import Data.IORef 
    (IORef, readIORef, writeIORef)
import Graphics.UI.Gtk 
    (ListStore, set, AttrOp(..), widgetSensitive)
import Graphics.UI.Gtk.Builder 
    (Builder)

import Guia.Gui.MainGladeGui
import Guia.Db.Database
import Guia.Db.Payer
import Guia.Db.BillingConcept

data MainState
    = View PanelId
    | Edit PanelId

-- TODO: use a record for this type
type AppData = ( MainState
               , MainGui
               , Builder                 -- Glade builder
               , Connection              -- DB connection
               , Maybe (ListStore Payer) -- payersLs
               , Maybe (ListStore BillingConcept)
               )

mainState :: AppData -> MainState
mainState (s, _, _, _, _, _) = s

mainGui :: AppData -> MainGui
mainGui (_, g, _, _, _, _) = g

builder :: AppData -> Builder
builder (_, _, b, _, _, _) = b

connection :: AppData -> Connection
connection (_, _, _, c, _, _) = c

payersLs :: AppData -> Maybe (ListStore Payer)
payersLs (_, _, _, _, p, _) = p

billingConceptsLs :: AppData -> Maybe (ListStore BillingConcept)
billingConceptsLs (_, _, _, _, _, b) = b

setMainState :: MainState -> (IORef AppData) -> IO ()
setMainState (View p) adRef = do 
  ad <- readIORef adRef
  let gui = mainGui ad
  set (mwExitBt gui) [widgetSensitive := True]
  mapM_ (flip set [widgetSensitive := True]) (otherChoosers p gui)
  writeIORef adRef (View p, gui, builder ad, connection ad, payersLs ad,
                    billingConceptsLs ad)
  putStrLn $ "View " ++ p
setMainState (Edit p) adRef = do 
  ad <- readIORef adRef
  let gui = mainGui ad
  set (mwExitBt gui) [widgetSensitive := False]
  mapM_ (flip set [widgetSensitive := False]) (otherChoosers p gui)
  writeIORef adRef (Edit p, gui, builder ad, connection ad, payersLs ad,
                    billingConceptsLs ad)
  putStrLn $ "Edit " ++ p
