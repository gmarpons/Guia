{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}

module Guia.Utils
    (State(..), setState, connectItemPanel,
     mkTreeViewModel, mkComboBoxModel, treeViewGetSortedModel, treeViewGetListStore)
where

import Data.List
    (isInfixOf)
import Data.Maybe
    (isNothing, fromJust)
import Control.Monad
    (ap)
import System.Glib.GObject
    (toGObject, unsafeCastGObject)
import Graphics.UI.Gtk
    -- (TreeView, ListStore, listStoreNew, treeViewSetModel, treeViewGetColumns,
    --  treeViewColumnGetCellRenderers, castToCellRendererText,
    --  cellLayoutSetAttributes, cellText, AttrOp(..), treeViewGetModel)
import Data.IORef
    (IORef, newIORef, readIORef, writeIORef)
import Data.Either
    (either)

import Guia.AppData
import Guia.Db.Item
import Guia.Gui.GuiUtils
import Guia.Gui.MainGladeGui

data State
    = NoSel
    | Sel     TreeIter
    | EditNew [String] Bool     -- ^ @Bool@ states if edited info is valid.
    | EditOld TreeIter Bool
    deriving Show

-- | Actions associated to state transitions. Pattern-matching is
-- non-exhaustive: function fails for meaningless transitions.
t :: GuiClass g i => State -> State -> (IORef AppData) -> g -> IO ()
t NoSel            NoSel            adRef gui = return ()
t NoSel            (Sel item)       adRef gui = return ()
t NoSel            (EditNew s b)    adRef gui = return ()
t (Sel _)          (Sel item)       adRef gui = return ()
t (Sel item)       NoSel            adRef gui = return ()
t (Sel _)          (EditOld item b) adRef gui = return ()
t (Sel _)          (EditNew s b)    adRef gui = cleanEntries gui
t (EditNew s b)    NoSel            adRef gui = return () -- Cancel Bt
t (EditNew s b)    (Sel _)          adRef gui = return () -- Cancel Bt
t (EditNew _ _)    (EditNew s b)    adRef gui = return ()
t (EditOld item b) (Sel _)          adRef gui = return () -- Cancel Bt
t (EditOld _ _)    (EditOld item b) adRef gui = return ()

-- | Actions associated to arriving at a state.
s :: GuiClass g i => State -> (IORef AppData) -> g -> String -> IO()
s NoSel         adRef gui pi = do cleanEntries gui
                                  mapM_ ((`set` [widgetSensitive:=False]) . ($ gui)) 
                                        editEntries
                                  mapM_ ((`set` [widgetSensitive:=False]) . ($ gui))
                                        editWidgets
                                  mapM_ ((`set` [widgetSensitive:=False]) . ($ gui))
                                        selectWidgets
                                  set (either toWidget toWidget (selector gui))
                                                    [widgetSensitive := True]
                                  set (editTb gui)   [widgetSensitive := False]
                                  set (newTb gui)    [widgetSensitive := True]
                                  set (deleteBt gui) [widgetSensitive := False]
                                  set (saveBt gui)   [widgetSensitive := False]
                                  set (cancelBt gui) [widgetSensitive := False]
                                  set (editTb gui)   [toggleButtonActive := False]
                                  set (newTb gui)    [toggleButtonActive := False]
                                  setMainState (View pi) adRef
s (Sel i)       adRef gui pi = do model <- either treeViewGetListStore 
                                           comboBoxGetListStore (selector gui)
                                  item <- (treeModelGetRow model i)
                                  putItem item gui
                                  mapM_ ((`set` [widgetSensitive:=False]) . ($ gui)) 
                                        editEntries
                                  mapM_ ((`set` [widgetSensitive:=False]) . ($ gui))
                                        editWidgets
                                  mapM_ ((`set` [widgetSensitive:=True]) . ($ gui))
                                        selectWidgets
                                  set (either toWidget toWidget (selector gui))
                                                    [widgetSensitive := True]
                                  set (editTb gui)   [widgetSensitive := True]
                                  set (newTb gui)    [widgetSensitive := True]
                                  set (deleteBt gui) [widgetSensitive := True]
                                  set (saveBt gui)   [widgetSensitive := False]
                                  set (cancelBt gui) [widgetSensitive := False]
                                  set (editTb gui)   [toggleButtonActive := False]
                                  set (newTb gui)    [toggleButtonActive := False]
                                  setMainState (View pi) adRef
s (EditNew i v) adRef gui pi = do mapM_ ((`set` [widgetSensitive:=True]) . ($ gui)) 
                                        editEntries
                                  mapM_ ((`set` [widgetSensitive:=True]) . ($ gui))
                                        editWidgets
                                  mapM_ ((`set` [widgetSensitive:=False]) . ($ gui))
                                        selectWidgets
                                  set (either toWidget toWidget (selector gui))
                                                    [widgetSensitive := False]
                                  set (editTb gui)   [widgetSensitive := False]
                                  set (newTb gui)    [widgetSensitive := False]
                                  set (deleteBt gui) [widgetSensitive := False]
                                  set (saveBt gui)   [widgetSensitive := v]
                                  set (cancelBt gui) [widgetSensitive := True]
                                  setMainState (Edit pi) adRef
s (EditOld i v) adRef gui pi = do mapM_ ((`set` [widgetSensitive:=True]) . ($ gui)) 
                                        editEntries
                                  mapM_ ((`set` [widgetSensitive:=True]) . ($ gui))
                                        editWidgets
                                  mapM_ ((`set` [widgetSensitive:=False]) . ($ gui))
                                        selectWidgets
                                  set (either toWidget toWidget (selector gui))
                                                    [widgetSensitive := False]
                                  set (editTb gui)   [widgetSensitive := False]
                                  set (newTb gui)    [widgetSensitive := False]
                                  set (deleteBt gui) [widgetSensitive := False]
                                  set (saveBt gui)   [widgetSensitive := v]
                                  set (cancelBt gui) [widgetSensitive := True]
                                  setMainState (Edit pi) adRef

setState :: GuiClass guiT itemT =>
            State
         -> IORef AppData 
         -> IORef State 
         -> guiT
         -> String -> IO ()
setState newSt adRef stRef gui panelId= do
  oldSt <- readIORef stRef
  t oldSt newSt adRef gui
  s newSt adRef gui panelId
  writeIORef stRef newSt
  putStrLn $ show newSt

eitherSelectorSetWidgetSensitive :: Either TreeView ComboBox -> Bool -> IO ()
eitherSelectorSetWidgetSensitive e b =
    either (flip set [widgetSensitive := b]) (flip set [widgetSensitive := b]) e

mkTreeViewModel :: forall a .
                   TreeView 
                -> Maybe (ListStore a) -- ^ If @Nothing@, a new model is created.
                -> [a -> String]       -- ^ One function per pair (column,
                                       -- renderer), as some columns can
                                       -- have more than one renderer
                -> IO (ListStore a)
mkTreeViewModel view mModel funcs = do
  model <- if isNothing mModel 
           then listStoreNew ([] :: [a]) 
           else return $ fromJust mModel
  sortedModel <- treeModelSortNewWithModel model
  treeViewSetModel view sortedModel
  columns <- treeViewGetColumns view
  mapM_ (\(col, func, sortColumnId) -> do
           renderers <- treeViewColumnGetCellRenderers (col)
           mapM_ (\renderer -> do
                    let rendText = castToCellRendererText renderer
                    cellLayoutSetAttributes col rendText model
                                                (\row -> [cellText := func row])
                 ) renderers
           treeViewColumnSetSortIndicator col True
           let sortFunc xIter yIter = do
                 xRow <- customStoreGetRow model xIter
                 yRow <- customStoreGetRow model yIter
                 return $ compare (func xRow) (func yRow)
           treeSortableSetSortFunc sortedModel sortColumnId sortFunc
           treeViewColumnSetSortColumnId col sortColumnId
        ) $ zip3 columns funcs [0..]
  -- Enable incremental search in TreeView
  let equalFunc text iter = do
        childIter <- treeModelSortConvertIterToChildIter sortedModel iter
        row <- customStoreGetRow model childIter
        let rowTexts = map ($ row) funcs
        return $ text `isInfixOf` foldl (++) "" rowTexts
  treeViewSetSearchEqualFunc view (Just equalFunc)
  return model

mkComboBoxModel :: forall a .
                   ComboBox
                -> (a -> String)       -- ^ One function per pair (column,
                                       -- renderer), as some columns can
                                       -- have more than one renderer
                -> IO (ListStore a)
mkComboBoxModel view func = do
  store <- listStoreNew ([] :: [a])
  comboBoxSetModel view (Just store)
  ren <- cellRendererTextNew
  cellLayoutPackStart view ren False
  cellLayoutSetAttributes view ren store (\row -> [cellText := func row])
  return store

-- | Possibly unsafe operation. Error if @view@ doesn't have a
-- TreeModelSort model.
treeViewGetSortedModel :: forall a . TreeView -> IO (TypedTreeModelSort a)
treeViewGetSortedModel view = do
  (Just uncastedTreeModelSort) <- treeViewGetModel view
  let treeModelSort = 
          ((unsafeCastGObject . toGObject) uncastedTreeModelSort)
                              :: (TypedTreeModelSort a)
  return treeModelSort

-- | Possibly unsafe operation. Error if @view@ doesn't have a
-- ListStore model.
treeViewGetListStore :: forall a . TreeView -> IO (ListStore a)
treeViewGetListStore view = do
  (Just uncastedTreeModelSort) <- treeViewGetModel view
  let treeModelSort = 
          ((unsafeCastGObject . toGObject) uncastedTreeModelSort) :: TreeModelSort
  uncastedListStore <- treeModelSortGetModel treeModelSort
  let listStore = (unsafeCastGObject . toGObject) uncastedListStore
  return listStore

comboBoxGetListStore :: forall a . ComboBox -> IO (ListStore a)
comboBoxGetListStore view = do
  (Just uncastedTreeModel) <- comboBoxGetModel view
  let listStore = (unsafeCastGObject . toGObject) uncastedTreeModel
  return listStore

selectItem :: Either TreeView ComboBox -> TreeIter -> IO ()
selectItem = either selectItemInTreeView selectItemInComboBox
    where selectItemInTreeView treeView iter = do
            sortModel <- treeViewGetSortedModel treeView
            sortIter <- treeModelSortConvertChildIterToIter sortModel iter
            selection <- treeViewGetSelection treeView
            treeSelectionSelectIter selection sortIter
          selectItemInComboBox = comboBoxSetActiveIter

connectItemPanel :: GuiClass guiT itemT =>
                    IORef AppData 
                 -> guiT
                 -> IORef State
                 -> String 
                 -> ([String] -> Bool) 
                 -> ListStore itemT
                 -- -> (IO (Maybe TreeIter))
                 -> IO ()
connectItemPanel adRef gui stRef panelId validStrings model  = do
  setState NoSel adRef stRef gui panelId
  ad <- readIORef adRef

  case (selector gui) of
    (Left treeView) -> do
      sortModel <- treeViewGetSortedModel treeView
      selection <- treeViewGetSelection treeView
      let toChildIter = treeModelSortConvertIterToChildIter sortModel
      let onSelectionChangedAction = do
                          count <- treeSelectionCountSelectedRows selection
                          if count == 0
                            then setState NoSel adRef stRef gui panelId
                            else treeSelectionSelectedForeach selection $ \it -> 
                                     do cIt <- toChildIter it
                                        -- row <- treeModelGetRow model cIt
                                        setState (Sel cIt) adRef stRef gui panelId
      on selection treeSelectionSelectionChanged $ onSelectionChangedAction
      on (cancelBt gui) buttonActivated $ onSelectionChangedAction
      return ()
    (Right comboBox) -> do
      let onSelectionChangedAction = do
                          mIt <- comboBoxGetActiveIter comboBox
                          case mIt of
                            (Just it) -> do 
                                       -- row <- treeModelGetRow model it
                                       setState (Sel it) adRef stRef gui panelId
                                       putStrLn "Invoice selected"
                            Nothing   -> putStrLn "No invoicing selected"
      on comboBox changed $ onSelectionChangedAction
      on (cancelBt gui) buttonActivated $ onSelectionChangedAction
      return ()
                        
  on (editTb gui) toggled $ do 
    isActive <- toggleButtonGetActive (editTb gui)
    if isActive
      then do
        Sel iter <- readIORef stRef
        item <- treeModelGetRow model iter
        let valid = validStrings (itemToStrings item)
        setState (EditOld iter valid) adRef stRef gui panelId
      else do
        return ()

  on (newTb gui) toggled $ do
    isActive <- toggleButtonGetActive (newTb gui)
    if isActive
      then do
        let s = guiNewItemToStrings gui
        setState (EditNew s $ validStrings s) adRef stRef gui panelId
      else do
        return ()

  on (deleteBt gui) buttonActivated $ do
    (Sel iter) <- readIORef stRef
    -- TODO: improve dialog message and create a singleton dialog
    dialog <- messageDialogNew (Just $ mainWd (mainGui ad)) 
              [DialogModal]
              MessageQuestion
              ButtonsYesNo
              "Estàs segur d'esborrar l'element?"
    response <- dialogRun dialog
    case response of
      ResponseYes -> do item <- treeModelGetRow model iter
                        deleteItemOnDb item (connection ad)
                        let index = listStoreIterToIndex iter
                        listStoreRemove model index
                        putStrLn $ "Delete"
                        setState (NoSel) adRef stRef gui panelId
      _           -> do putStrLn "Other response."
    widgetHideAll dialog

  on (saveBt gui) buttonActivated $ do
    st <- readIORef stRef
    is <- getItemStrings gui
    iter <- case st of
      EditNew _ True ->    do putStrLn "Inserting new."
                              strings <- guiReadItemIntoStrings gui
                              new <- insertItemFromStringsOnDb strings (connection ad)
                              index <- listStoreAppend model new
                              let treePath = stringToTreePath (show index)
                              Just iter <- treeModelGetIter model treePath
                              selectItem (selector gui) iter
                              return iter
      EditOld iter True -> do putStrLn "Updating old."
                              old <- treeModelGetRow model iter
                              putStrLn "Updating old 2."
                              new <- updateItemOnDb is old (connection ad)
                              putStrLn "Updating old 3."
                              let index = listStoreIterToIndex iter
                              listStoreSetValue model index new
                              putStrLn "Updating old 4."
                              return iter
    setState (Sel iter) adRef stRef gui panelId

  let onEditableChangedAction = do
        st <- readIORef stRef
        p <- guiReadItemIntoStrings gui
        case (st, validStrings p) of
          (EditNew s old, new) | new /= old ->
                setState (EditNew s new) adRef stRef gui panelId
          (EditOld i old, new) | new /= old ->
                setState (EditOld i new) adRef stRef gui panelId
          (_, _)             -> putStrLn "Res"

  mapM_ (($ onEditableChangedAction) . (`on` editableChanged) . ($ gui)) editEntries
  
  return ()
