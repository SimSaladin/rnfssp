{-# LANGUAGE OverloadedStrings #-}

module Main where

import TestImport
import Test.Hspec (hspec)

tom = MediaSubR

mediaSpecs :: Spec
mediaSpecs = 
    ydescribe "Media tests" $ do

        yit "loads media home page" $ do
            get (tom MediaHomeR)
            statusIs 200

main :: IO ()
main = do
    foundation <- makeFoundation
    hspec $
        yesodSpec foundation mediaSpecs
