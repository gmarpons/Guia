{-# LANGUAGE MultiParamTypeClasses #-}

module Guia.Gui.InvoicingsGladeGui (InvoicingsGui(..), mkInvoicingsGui)
where

import Graphics.UI.Gtk.Builder (Builder, builderGetObject)
import Graphics.UI.Gtk (castToButton, castToTreeView, castToEntry, castToComboBox,
                        castToToggleButton, Button, TreeView, 
                        Entry, ComboBox, ToggleButton, TreeSelection,
                        treeViewGetSelection, treeSelectionSetMode,
                        SelectionMode(..), toWidget)

import Guia.Gui.GuiUtils
import Guia.Db.Invoicing

data InvoicingsGui = InvoicingsGui
    { invoicingsCb :: ComboBox
    , editInvoicingTb :: ToggleButton
    , cloneInvoicingBt :: Button
    , newInvoicingTb :: ToggleButton
    , deleteInvoicingBt :: Button
    , printInvoicingBt :: Button
    , saveInvoicingBt :: Button
    , cancelInvoicingBt :: Button
    , invoicingDescriptionEn :: Entry
    , invoicingEditionDateEn :: Entry
    , chargeActualBasePriceEn :: Entry
    , addChargeBt :: Button
    , deleteChargeBt :: Button
    , invoicingBasePriceEn :: Entry
    , invoicingFinalPriceEn :: Entry
    , invoicingNumberOfInvoicesEn :: Entry
    , payersInInvoicingTv :: TreeView
    , payersInInvoicingSl :: TreeSelection
    , billingConceptsInInvoicingTv :: TreeView
    , billingConceptsInInvoicingSl :: TreeSelection
    , chargesTv :: TreeView
    , chargesSl :: TreeSelection
    }

instance GuiClass InvoicingsGui Invoicing where
    editEntries   = [ invoicingDescriptionEn
                    , invoicingEditionDateEn
                    , invoicingBasePriceEn
                    , invoicingFinalPriceEn
                    , invoicingNumberOfInvoicesEn ]
    editWidgets   = [ toWidget . payersInInvoicingTv
                    , toWidget . billingConceptsInInvoicingTv
                    , toWidget . chargesTv ]
    selectWidgets = [ toWidget . cloneInvoicingBt
                    , toWidget . printInvoicingBt ]
    newTb         = newInvoicingTb
    editTb        = editInvoicingTb
    deleteBt      = deleteInvoicingBt
    saveBt        = saveInvoicingBt
    cancelBt      = cancelInvoicingBt
    selector      = Right . invoicingsCb

mkInvoicingsGui :: Builder -> IO InvoicingsGui
mkInvoicingsGui builder = do
  invoicingsCb <- builderGetObject builder castToComboBox "invoicingsCb"
  editInvoicingTb <- builderGetObject builder castToToggleButton "editInvoicingTb"
  cloneInvoicingBt <- builderGetObject builder castToButton "cloneInvoicingBt"
  newInvoicingTb <- builderGetObject builder castToToggleButton "newInvoicingTb"
  deleteInvoicingBt <- builderGetObject builder castToButton "deleteInvoicingBt"
  printInvoicingBt <- builderGetObject builder castToButton "printInvoicingBt"
  saveInvoicingBt <- builderGetObject builder castToButton "saveInvoicingBt"
  cancelInvoicingBt <- builderGetObject builder castToButton "cancelInvoicingBt"
  invoicingDescriptionEn 
      <- builderGetObject builder castToEntry "invoicingDescriptionEn"
  invoicingEditionDateEn 
      <- builderGetObject builder castToEntry "invoicingEditionDateEn"
  chargeActualBasePriceCe 
      <- builderGetObject builder castToEntry "chargeActualBasePriceEn"
  addChargeBt <- builderGetObject builder castToButton "addChargeBt"
  deleteChargeBt <- builderGetObject builder castToButton "deleteChargeBt"
  invoicingBasePriceEn <- builderGetObject builder castToEntry "invoicingBasePriceEn"
  invoicingFinalPriceEn 
      <- builderGetObject builder castToEntry "invoicingFinalPriceEn"
  invoicingNumberOfInvoicesEn
      <- builderGetObject builder castToEntry "invoicingNumberOfInvoicesEn"

  payersInInvoicingTv 
      <- builderGetObject builder castToTreeView "payersInInvoicingTv"
  payersInInvoicingSl <- treeViewGetSelection payersInInvoicingTv
  treeSelectionSetMode payersInInvoicingSl SelectionSingle
  mkTreeViewColumns payersInInvoicingTv ["Cognoms", "Nom"]

  billingConceptsInInvoicingTv 
      <- builderGetObject builder castToTreeView "billingConceptsInInvoicingTv"
  billingConceptsInInvoicingSl <- treeViewGetSelection billingConceptsInInvoicingTv
  treeSelectionSetMode billingConceptsInInvoicingSl SelectionSingle
  mkTreeViewColumns billingConceptsInInvoicingTv [ "Descripció", "Preu base"
                                                 , "Preu final"]

  chargesTv <- builderGetObject builder castToTreeView "chargesTv"
  chargesSl <- treeViewGetSelection chargesTv
  treeSelectionSetMode chargesSl SelectionSingle
  mkTreeViewColumns chargesTv ["Cognoms", "Concepte", "Preu base"]

  return (InvoicingsGui invoicingsCb editInvoicingTb cloneInvoicingBt newInvoicingTb
                        deleteInvoicingBt printInvoicingBt
                        saveInvoicingBt cancelInvoicingBt
                        invoicingDescriptionEn invoicingEditionDateEn 
                        chargeActualBasePriceCe addChargeBt deleteChargeBt 
                        invoicingBasePriceEn invoicingFinalPriceEn
                        invoicingNumberOfInvoicesEn 
                        payersInInvoicingTv payersInInvoicingSl
                        billingConceptsInInvoicingTv billingConceptsInInvoicingSl
                        chargesTv chargesSl
         )
