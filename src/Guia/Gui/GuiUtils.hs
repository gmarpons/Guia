{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Guia.Gui.GuiUtils
    (GuiClass(..), guiNewItemToStrings, guiReadItemIntoStrings, mkTreeViewColumns)
where

import Graphics.UI.Gtk
    (TreeView, treeViewColumnNew, treeViewColumnSetTitle, treeViewAppendColumn,
     treeViewColumnSetSizing, TreeViewColumnSizing(..), cellRendererTextNew,
     cellLayoutPackStart, treeViewColumnSetResizable, treeViewColumnSetSortIndicator,
     treeViewColumnsAutosize, Entry, AttrOp(..), entryText, get, set,
     ToggleButton, Button, TreeSelection, ComboBox, cellLayoutPackEnd,
     Widget)

import Guia.Db.Item

class (ItemClass i) => GuiClass g i | g -> i where
    editEntries        :: [g -> Entry]
    editWidgets        :: [g -> Widget]
    editWidgets        =  []
    selectWidgets      :: [g -> Widget]
    selectWidgets      =  []
    cleanEntries       :: g -> IO ()
    cleanEntries gui   =  mapM_ ((`set` [entryText := ""]) . ($ gui)) editEntries
    putItem            :: i -> g -> IO ()
    putItem it gui     =  mapM_ (\(entry, text) -> do
                                   set (entry gui) [entryText := text]
                                ) $ zip editEntries (itemToStrings it)
    getItemStrings     :: g -> IO [String]
    getItemStrings gui =  mapM ((`get` entryText) . ($ gui)) $ editEntries
    newTb              :: g -> ToggleButton
    editTb             :: g -> ToggleButton
    deleteBt           :: g -> Button
    saveBt             :: g -> Button
    cancelBt           :: g -> Button
    selector           :: g -> Either TreeView ComboBox

guiNewItemToStrings :: GuiClass g i => g -> [String]
guiNewItemToStrings gui = map ((const "") . ($ gui)) editEntries

guiReadItemIntoStrings :: GuiClass g i => g -> IO ([String])
guiReadItemIntoStrings gui = do
  mapM ((`get` entryText) . ($ gui)) editEntries

mkTreeViewColumns :: TreeView -> [String] -> IO ()
mkTreeViewColumns view titles = do
  mapM_ (\title -> do 
           col <- treeViewColumnNew
           treeViewColumnSetTitle col title
           num <- treeViewAppendColumn view col
           treeViewColumnSetSizing col (if num == length titles
                                          then TreeViewColumnFixed
                                          else TreeViewColumnAutosize)
           rd <- cellRendererTextNew
           cellLayoutPackStart col rd (if num == length titles then False else True)
           treeViewColumnSetResizable col True
        ) $ titles
  treeViewColumnsAutosize view

-- | Only one column combo box.
mkComboBoxColumns :: ComboBox -> IO ()
mkComboBoxColumns view = do
  rd <- cellRendererTextNew
  cellLayoutPackEnd view rd True
