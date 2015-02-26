{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Snap.Extras.Ajax
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman
-- Stability   :  experimental
--
-- Simple combinators to work with AJAX requests.
----------------------------------------------------------------------------

module Snap.Extras.Ajax
    ( replaceWith
    , replaceWithTemplate
    , ResponseType (..)
    , respond
    , responds
    , htmlOrAjax
    ) where

-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Applicative
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as B
import           Data.Text
import qualified Data.Text                  as T
import           Heist.Compiled
import           Language.Javascript.JMacro
import           Safe
import           Snap.Core
import           Snap.Extras.CoreUtils
import           Snap.Snaplet
import           Snap.Snaplet.Heist
-------------------------------------------------------------------------------


-- | Replace innerHTML of given selector with given conntent.
replaceWith
    :: MonadSnap m
    => Text
    -- ^ jquery selector
    -> ByteString
    -- ^ content blob
    -> m ()
replaceWith selector bs = do
    let bs' = B.unpack bs
        sel = T.unpack selector
    jsResponse
    writeBS $ B.pack $ show . renderJs $ replaceWithJs bs' sel


-- | Produce JS statement to replace a target's inner with given
-- contents.
replaceWithJs :: String -> String -> JStat
replaceWithJs bs sel = [jmacro|
    var contents = `(bs)`;
    var replace = function() { $(`(sel)`).html(contents); };
    replace();
|]


-------------------------------------------------------------------------------
-- | Replace the inner HTML element of a given selector with the
-- contents of the rendered Heist template.
--
-- Currently expect you to have jQuery loaded.
-- TODO: Make this jQuery independent
replaceWithTemplate
    :: HasHeist v
    => ByteString
    -- ^ Heist template name
    -> Text
    -- ^ jQuery selector for target element on page
    -> Handler v v ()
replaceWithTemplate nm sel = do
    (bld, _) <- maybeBadReq "Could not render a response." $
      withHeistState $ \ hs -> renderTemplate hs nm
    bld' <- bld
    replaceWith sel (toByteString bld')



-------------------------------------------------------------------------------
-- | Possible reponse types we support at the moment. Can be expanded
-- for more use cases like JSON, CSV and XML.
data ResponseType = Html | Ajax
  deriving (Eq,Show,Read,Ord)


-------------------------------------------------------------------------------
-- | The multi-mime dispatcher. It will inspect the 'Accept' header
-- and determine what type of a request this is. If AJAX, make sure to
-- set the Accept header to 'application/javascript'.
respond :: MonadSnap m => (ResponseType -> m b) -> m b
respond f = do
    hs <- maybeBadReq "Accept header required for this handler" $
          getHeader "accept" <$> getRequest
    if B.isInfixOf "application/javascript" hs
      then f Ajax
      else f Html


-- | Dispatch on all response types
responds :: MonadSnap m => [(ResponseType, m b)] -> m b
responds fs = respond $ \ ty -> fromJustNote ("Handler does not know how to respond to: " ++ show ty) (lookup ty fs)


-- | Classic pattern of responding to a static HTML or an alternative
-- AJAX request.
htmlOrAjax
    :: MonadSnap m
    => m b
    -- ^ If call is HTML
    -> m b
    -- ^ If call is AJAX
    -> m b
htmlOrAjax f g = respond $ \ ty -> case ty of
    Html -> f
    Ajax -> g


