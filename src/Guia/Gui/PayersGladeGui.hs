{-# LANGUAGE MultiParamTypeClasses #-}

module Guia.Gui.PayersGladeGui 
    (PayersGui(..), GuiClass(..), mkPayersGui, mkTreeViewColumns)
where

import Graphics.UI.Gtk.Builder 
    (Builder, builderGetObject)
import Graphics.UI.Gtk 
    (castToButton, castToTreeView, castToEntry, castToToggleButton,
     Button, TreeView, Entry, ToggleButton, TreeSelection,
     treeViewGetSelection, treeSelectionSetMode, SelectionMode(..))

import Guia.Gui.GuiUtils
import Guia.Db.Payer

data PayersGui = PayersGui
    { payersTv :: TreeView
    , editPayerTb :: ToggleButton
    , newPayerTb :: ToggleButton
    , deletePayerBt :: Button
    , personNameFirstEn :: Entry
    , personNameLastEn :: Entry
    , bankAccountBankIdEn :: Entry
    , bankAccountOfficeEn :: Entry
    , bankAccountControlDigitsEn :: Entry
    , bankAccountNumEn :: Entry
    , oldBankAccountsBt :: Button
    , savePayerBt :: Button
    , cancelPayerBt :: Button
    , payersSl :: TreeSelection
    }

instance GuiClass PayersGui Payer where
    editEntries  = [ personNameFirstEn
                   , personNameLastEn
                   , bankAccountBankIdEn
                   , bankAccountOfficeEn
                   , bankAccountControlDigitsEn
                   , bankAccountNumEn ]
    newTb        = newPayerTb
    editTb       = editPayerTb
    deleteBt     = deletePayerBt
    saveBt       = savePayerBt
    cancelBt     = cancelPayerBt
    selector     = Left . payersTv
    -- mSelection    = Just . payersSl

mkPayersGui :: Builder -> IO PayersGui
mkPayersGui builder = do
  payersTv <- builderGetObject builder castToTreeView "payersTv"
  editPayerTb <- builderGetObject builder castToToggleButton "editPayerTb"
  newPayerTb <- builderGetObject builder castToToggleButton "newPayerTb"
  deletePayerBt <- builderGetObject builder castToButton "deletePayerBt"
  personNameFirstEn <- builderGetObject builder castToEntry "personNameFirstEn"
  personNameLastEn <- builderGetObject builder castToEntry "personNameLastEn"
  bankAccountBankIdEn <- builderGetObject builder castToEntry "bankAccountBankIdEn"
  bankAccountOfficeEn <- builderGetObject builder castToEntry "bankAccountOfficeEn"
  bankAccountControlDigitsEn 
      <- builderGetObject builder castToEntry "bankAccountControlDigitsEn"
  bankAccountNumEn <- builderGetObject builder castToEntry "bankAccountNumEn"
  oldBankAccountsBt <- builderGetObject builder castToButton "oldBankAccountsBt"
  savePayerBt <- builderGetObject builder castToButton "savePayerBt"
  cancelPayerBt <- builderGetObject builder castToButton "cancelPayerBt"
  payersSl <- treeViewGetSelection payersTv
  treeSelectionSetMode payersSl SelectionSingle
  mkTreeViewColumns payersTv ["Cognoms", "Nom", "Data d'inscripció"]
  return (PayersGui payersTv editPayerTb newPayerTb deletePayerBt personNameFirstEn
                    personNameLastEn bankAccountBankIdEn bankAccountOfficeEn
                    bankAccountControlDigitsEn bankAccountNumEn oldBankAccountsBt
                    savePayerBt cancelPayerBt payersSl
         )
