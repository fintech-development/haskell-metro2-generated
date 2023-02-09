{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import METRO2.Model
import Network.Integrated.HTTP.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy BaseSegment)
      propMimeEq MimeJSON (Proxy :: Proxy DataRecord)
      propMimeEq MimeJSON (Proxy :: Proxy File)
      propMimeEq MimeJSON (Proxy :: Proxy HeaderRecord)
      propMimeEq MimeJSON (Proxy :: Proxy J1Segment)
      propMimeEq MimeJSON (Proxy :: Proxy J2Segment)
      propMimeEq MimeJSON (Proxy :: Proxy K1Segment)
      propMimeEq MimeJSON (Proxy :: Proxy K2Segment)
      propMimeEq MimeJSON (Proxy :: Proxy K3Segment)
      propMimeEq MimeJSON (Proxy :: Proxy K4Segment)
      propMimeEq MimeJSON (Proxy :: Proxy L1Segment)
      propMimeEq MimeJSON (Proxy :: Proxy N1Segment)
      propMimeEq MimeJSON (Proxy :: Proxy TrailerRecord)
      
