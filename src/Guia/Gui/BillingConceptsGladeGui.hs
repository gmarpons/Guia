{-# LANGUAGE MultiParamTypeClasses #-}

module Guia.Gui.BillingConceptsGladeGui 
    (BillingConceptsGui(..), GuiClass(..), mkBillingConceptsGui, mkTreeViewColumns)
where

import Graphics.UI.Gtk.Builder 
    (Builder, builderGetObject)
import Graphics.UI.Gtk 
    (castToButton, castToTreeView, castToEntry,
     castToToggleButton, Button, TreeView, Entry, ToggleButton,
     TreeSelection, treeViewGetSelection, treeSelectionSetMode,
     SelectionMode(..))

import Guia.Gui.GuiUtils
import Guia.Db.BillingConcept

data BillingConceptsGui = BillingConceptsGui
    { billingConceptsTv :: TreeView
    , editBillingConceptTb :: ToggleButton
    , newBillingConceptTb :: ToggleButton
    , deleteBillingConceptBt :: Button
    , billingConceptDescriptionEn :: Entry
    , billingConceptBasePriceEn :: Entry
    , billingConceptVatRatioEn :: Entry
    , billingConceptFinalPriceEn :: Entry
    , saveBillingConceptBt :: Button
    , cancelBillingConceptBt :: Button
    , billingConceptsSl :: TreeSelection
    }

instance GuiClass BillingConceptsGui BillingConcept where
    editEntries  = [ billingConceptDescriptionEn
                   , billingConceptBasePriceEn
                   , billingConceptVatRatioEn
                   , billingConceptFinalPriceEn ]
    newTb        = newBillingConceptTb
    editTb       = editBillingConceptTb
    deleteBt     = deleteBillingConceptBt
    saveBt       = saveBillingConceptBt
    cancelBt     = cancelBillingConceptBt
    selector     = Left . billingConceptsTv
    -- mSelection    = Just . billingConceptsSl

mkBillingConceptsGui :: Builder -> IO BillingConceptsGui
mkBillingConceptsGui builder = do
  billingConceptsTv <- builderGetObject builder castToTreeView "billingConceptsTv"
  editBillingConceptTb 
      <- builderGetObject builder castToToggleButton "editBillingConceptTb"
  newBillingConceptTb 
      <- builderGetObject builder castToToggleButton "newBillingConceptTb"
  deleteBillingConceptBt 
      <- builderGetObject builder castToButton "deleteBillingConceptBt"
  billingConceptDescriptionEn 
      <- builderGetObject builder castToEntry "billingConceptDescriptionEn"
  billingConceptBasePriceEn
      <- builderGetObject builder castToEntry "billingConceptBasePriceEn"
  billingConceptVatRatioEn
      <- builderGetObject builder castToEntry "billingConceptVatRatioEn"
  billingConceptFinalPriceEn
      <- builderGetObject builder castToEntry "billingConceptFinalPriceEn"
  saveBillingConceptBt <- builderGetObject builder castToButton "saveBillingConceptBt"
  cancelBillingConceptBt 
      <- builderGetObject builder castToButton "cancelBillingConceptBt"
  billingConceptsSl <- treeViewGetSelection billingConceptsTv
  treeSelectionSetMode billingConceptsSl SelectionSingle
  mkTreeViewColumns billingConceptsTv 
                        ["Descripció", "Preu base", "% IVA", "Preu final"]
  return (BillingConceptsGui billingConceptsTv editBillingConceptTb 
                             newBillingConceptTb deleteBillingConceptBt 
                             billingConceptDescriptionEn
                             billingConceptBasePriceEn billingConceptVatRatioEn
                             billingConceptFinalPriceEn saveBillingConceptBt
                             cancelBillingConceptBt billingConceptsSl
         )
