{-
   METRO2 API

   Moov Metro2 ([Automated Clearing House](https://en.wikipedia.org/wiki/Automated_Clearing_House)) implements an HTTP API for creating, parsing and validating Metro2 files. Metro2 is an open-source consumer credit history report for credit report file creation and validation.

   OpenAPI Version: 3.0.2
   METRO2 API API version: v1
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : METRO2.API.Metro2Files
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module METRO2.API.Metro2Files where

import METRO2.Core
import METRO2.MimeTypes
import METRO2.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Metro2Files

-- *** convert

-- | @POST \/convert@
-- 
-- Convert metro2 file
-- 
-- Convert from original metro2 file to new metro2 file
-- 
convert
  :: (Consumes Convert contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> TheMetro2Request Convert contentType FilePath accept
convert _  _ =
  _mkRequest "POST" ["/convert"]

data Convert  

-- | /Optional Param/ "format" - format of metro file
instance HasOptionalParam Convert Format where
  applyOptionalParam req (Format xs) =
    req `_addMultiFormPart` NH.partLBS "format" (mimeRender' MimeMultipartFormData xs)

-- | /Optional Param/ "type" - metro file type
instance HasOptionalParam Convert ParamType where
  applyOptionalParam req (ParamType xs) =
    req `_addMultiFormPart` NH.partLBS "type" (mimeRender' MimeMultipartFormData xs)

-- | /Optional Param/ "generate" - generate new trailer record
instance HasOptionalParam Convert Generate where
  applyOptionalParam req (Generate xs) =
    req `_addMultiFormPart` NH.partLBS "generate" (mimeRender' MimeMultipartFormData xs)

-- | /Optional Param/ "newline" - has new line
instance HasOptionalParam Convert Newline where
  applyOptionalParam req (Newline xs) =
    req `_addMultiFormPart` NH.partLBS "newline" (mimeRender' MimeMultipartFormData xs)

-- | /Optional Param/ "file" - metro2 file to upload
instance HasOptionalParam Convert File2 where
  applyOptionalParam req (File2 xs) =
    req `_addMultiFormPart` NH.partFileSource "file" xs

-- | /Optional Param/ "format" - print metro2 file type
instance HasOptionalParam Convert Format2 where
  applyOptionalParam req (Format2 xs) =
    req `addQuery` toQuery ("format", Just xs)

-- | @application/json@
instance Consumes Convert MimeJSON
-- | @multipart/form-data@
instance Consumes Convert MimeMultipartFormData

-- | @application/octet-stream@
instance Produces Convert MimeOctetStream
-- | @text/plain@
instance Produces Convert MimePlainText


-- *** health

-- | @GET \/health@
-- 
-- health metro2 service
-- 
-- Check the metro2 service to check if running
-- 
health
  :: TheMetro2Request Health MimeNoContent Text MimePlainText
health =
  _mkRequest "GET" ["/health"]

data Health  
-- | @text/plain@
instance Produces Health MimePlainText


-- *** print

-- | @POST \/print@
-- 
-- Print metro2 file with specific format
-- 
-- Print metro2 file with requested file format.
-- 
print
  :: (Consumes Print contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> TheMetro2Request Print contentType Text accept
print _  _ =
  _mkRequest "POST" ["/print"]

data Print  

-- | /Optional Param/ "format" - print metro2 file type
instance HasOptionalParam Print Format where
  applyOptionalParam req (Format xs) =
    req `_addMultiFormPart` NH.partLBS "format" (mimeRender' MimeMultipartFormData xs)

-- | /Optional Param/ "file" - metro2 file to upload
instance HasOptionalParam Print File2 where
  applyOptionalParam req (File2 xs) =
    req `_addMultiFormPart` NH.partFileSource "file" xs

-- | /Optional Param/ "format" - print metro2 file type
instance HasOptionalParam Print Format2 where
  applyOptionalParam req (Format2 xs) =
    req `addQuery` toQuery ("format", Just xs)

-- | @application/json@
instance Consumes Print MimeJSON
-- | @multipart/form-data@
instance Consumes Print MimeMultipartFormData

-- | @application/json@
instance Produces Print MimeJSON
-- | @text/plain@
instance Produces Print MimePlainText


-- *** validator

-- | @POST \/validator@
-- 
-- Validate metro2 file
-- 
-- Validation of metro2 file.
-- 
validator
  :: (Consumes Validator MimeMultipartFormData)
  => TheMetro2Request Validator MimeMultipartFormData Text MimePlainText
validator =
  _mkRequest "POST" ["/validator"]

data Validator  

-- | /Optional Param/ "file" - metro2 file to upload
instance HasOptionalParam Validator File2 where
  applyOptionalParam req (File2 xs) =
    req `_addMultiFormPart` NH.partFileSource "file" xs

-- | @multipart/form-data@
instance Consumes Validator MimeMultipartFormData

-- | @text/plain@
instance Produces Validator MimePlainText

