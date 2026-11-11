{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Spec (tests, main) where

#insert_test_imports

import Test.Sandwich
import TestLib.Main


tests :: TopSpec
tests = $(getSpecFromFolder defaultGetSpecFromFolderOptions)

main :: IO ()
main = specMain tests
