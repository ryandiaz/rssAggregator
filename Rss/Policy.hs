{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings #-}

module Rss.Policy (
   RssPolicyModule
   , withRssPolicyModule
   ) where

import Data.Typeable

import LIO
import LIO.DCLabel
import Hails.Database
import Hails.PolicyModule
import Hails.PolicyModule.DSL

data RssPolicyModule = RssPolicyModuleTCB DCPriv deriving Typeable

instance PolicyModule RssPolicyModule where
  initPolicyModule priv = do
    setPolicy priv $ do
      database $ do
        readers ==> unrestricted 
        writers ==> unrestricted
        admins ==> this
      collection "users" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \_ -> do
          readers ==> unrestricted
          writers ==> unrestricted
        field "name" key
        field "_id" key
      collection "" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \_ -> do
          readers ==> unrestricted
          writers ==> unrestricted
        field "_id" key
      collection "" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \_ -> do
          readers ==> unrestricted
          writers ==> unrestricted
        field "_id" key
      collection "" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy ==> this
          integrity ==> unrestricted
        document $ \_ -> do
          readers ==> unrestricted
          writers ==> unrestricted
        field "_id" key
    return $ RssPolicyModuleTCB priv
        where this = privDesc priv

withRssPolicyModule :: DBAction a -> DC a
withRssPolicyModule act = withPolicyModule (\(_ :: RssPolicyModule) -> act)