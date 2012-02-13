module Guia.Gui.MainGladeGui 
    (MainGui(..), PanelId, getGuiBuilder, mkMainGui, getPanelIdFromChooser,
     otherChoosers)
where

import Graphics.UI.Gtk.Builder 
    (Builder, builderGetObject, builderNew, builderAddFromFile)
import Graphics.UI.Gtk 
    (castToWindow, castToButton, castToToggleButton, castToVBox,
     Window, Button, ToggleButton, VBox, on, toggled,
     toggleButtonGetActive, toggleButtonActive,
     widgetGetName, widgetSetName, set, AttrOp(..), 
     widgetSensitive, containerGetChildren,
     containerRemove, boxPackStart, Packing(PackGrow))

type PanelId = String
type PanelDescr = (PanelId, (String, String))

panels :: [PanelDescr]
panels =
    [ ("payers",          ("payersTb",          "payersVb"         ))
    , ("billingConcepts", ("billingConceptsTb", "billingConceptsVb"))
    , ("invoicings",      ("invoicingsTb",      "invoicingsVb"     ))
    ]

data MainGui = MainGui
    { mainWd :: Window
    , mainVb :: VBox
    , mwExitBt :: Button
    , panelIds :: [PanelId]
    , panelChoosers :: [ToggleButton]
    }

getGuiBuilder :: String -> IO Builder
getGuiBuilder gladepath = do
  -- Note: crashes with a runtime error on console if fails!
  builder <- builderNew
  builderAddFromFile builder gladepath
  return builder

mkMainGui :: Builder -> IO MainGui
mkMainGui builder = do
  mainWd <- builderGetObject builder castToWindow "mainWd"
  mainVb <- builderGetObject builder castToVBox "mainVb"
  mwExitBt <- builderGetObject builder castToButton "mwExitBt"
  let panelIds = (fst . unzip)  panels
  let panelChooserNames = (fst . unzip . snd . unzip) panels
  panelChoosers <- mapM (\n -> do w <- builderGetObject builder castToToggleButton n
                                  widgetSetName w n
                                  return w
                        ) panelChooserNames
  panelBoxes <- mapM (builderGetObject builder castToVBox) 
                     $ (snd . unzip . snd . unzip) panels
  set (head panelChoosers) [ toggleButtonActive := True, widgetSensitive := False ]
  mapM_ (\chs -> on chs toggled $ do
                   isActive <- toggleButtonGetActive chs
                   if isActive
                      then do chsName <- widgetGetName chs
                              putStrLn "toggled"
                              children <- containerGetChildren mainVb
                              let otherChs = filter (/= chs) panelChoosers
                                  oldBox = head children
                                  mbNewBox = lookup chsName 
                                             $ zip panelChooserNames panelBoxes
                                  (Just newBox) = mbNewBox
                              set chs [widgetSensitive := False]
                              mapM_ (\o -> do set o [ toggleButtonActive := False
                                                    , widgetSensitive := True
                                                    ]
                                    ) otherChs
                              containerRemove mainVb oldBox
                              boxPackStart mainVb newBox PackGrow 0
                      else return ()
        ) panelChoosers
  return (MainGui mainWd mainVb mwExitBt panelIds panelChoosers)

getPanelIdFromChooser :: MainGui -> ToggleButton -> PanelId
getPanelIdFromChooser gui chs =
    let (Just id) = lookup chs $ zip (panelChoosers gui) (panelIds gui)
    in id

otherChoosers :: PanelId -> MainGui -> [ToggleButton]
otherChoosers id gui = filter (/= chs) (panelChoosers gui)
    where (Just chs) = lookup id $ zip (panelIds gui) (panelChoosers gui)
