{-# LANGUAGE ScopedTypeVariables #-}

module Guia.Db.Item
    (ItemClass(..), genericValidStrings)
where

import Database.HDBC.PostgreSQL 
    (Connection)

class Show a => ItemClass a where
    itemToStrings             :: a -> [String]
    updateItemOnDb            :: [String] -> a -> Connection -> IO a
    insertItemFromStringsOnDb :: [String] -> Connection -> IO a
    deleteItemOnDb            :: a -> Connection -> IO ()

genericValidStrings :: [String -> Bool] -> ([String] -> Bool) -> [String] -> Bool
genericValidStrings localChecks globalCheck strings
    = (and $ (map (uncurry ($)) (zip localChecks strings))) && globalCheck strings
