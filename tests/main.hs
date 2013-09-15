{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import Yesod.Default.Config
import Yesod.Test
import Application (makeFoundation)

import HomeTest

import Sections.BackendGitAnnex
import Data.Conduit
import Data.Conduit.List as CL

-- TODO TODO TODO MOOOVE THIS!
gitTests = do
    gitSource "/home/media/anime" ["annex", "find"]
        $= gitSeparatePaths
        $$ CL.mapM_ print

main :: IO ()
main = do

    gitTests

--    conf <- Import.loadConfig $ (configSettings Testing) { csParseExtra = parseExtra }
--    foundation <- makeFoundation conf
--    app <- toWaiAppPlain foundation
--    runTests app (connPool foundation) homeSpecs
