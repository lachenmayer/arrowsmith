{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Snap.Extras.NavTrails where

import           Blaze.ByteString.Builder.ByteString
import           Control.Monad.State.Strict
import           Data.ByteString            (ByteString)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Heist
import qualified Heist.Compiled as C
import           Heist.Interpreted



-------------------------------------------------------------------------------
data NavTrail b = NavTrail {
      ntSes :: SnapletLens b SessionManager
      -- ^ A session manager for the base
    }


-------------------------------------------------------------------------------
initNavTrail :: SnapletLens b SessionManager
             -- ^ Lens to the session snaplet
             -> Maybe (Snaplet (Heist b))
             -- ^ The heist snaplet (not a lens), if you want splices to be
             -- added automatically.
             -> SnapletInit b (NavTrail b)
initNavTrail ses heist =
  makeSnaplet "NavTrail"
              "Makes it easier for you to navigate back to key app points."
              Nothing $ do
  maybe (return ()) addNavTrailSplices heist
  return $ NavTrail ses


-------------------------------------------------------------------------------
-- |
setFocus :: Handler b (NavTrail b) ()
setFocus = do
  setFocus' =<< rqURI `fmap` getRequest


-------------------------------------------------------------------------------
-- |
setFocus' :: ByteString -> Handler b (NavTrail b) ()
setFocus' uri = do
  sl <- gets ntSes
  withSession sl $ withTop sl $ do
    setInSession "_nt_focus" $ T.decodeUtf8 uri


-------------------------------------------------------------------------------
-- |
setFocusToRef :: Handler b (NavTrail b) ()
setFocusToRef = do
  sl <- gets ntSes
  (maybe "/" id . getHeader "Referer") `fmap` getRequest >>=
    withTop sl . setInSession "_nt_focus" . T.decodeUtf8


-------------------------------------------------------------------------------
-- |
getFocus :: Handler b (NavTrail b) (Maybe Text)
getFocus = do
  sl <- gets ntSes
  withTop sl (getFromSession "_nt_focus")


getFocusDef :: Text -> Handler b (NavTrail b) Text
getFocusDef def = (fromJust . (`mplus` Just def)) `fmap` getFocus



-------------------------------------------------------------------------------
-- |
redirBack :: MonadSnap m => m a
redirBack = redirect =<< (maybe "/" id . getHeader "Referer") `fmap` getRequest


-------------------------------------------------------------------------------
-- |
redirFocus :: ByteString -> Handler b (NavTrail b) a
redirFocus def = do
  f <- (`mplus` Just def) `fmap` (fmap T.encodeUtf8 `fmap` getFocus)
  redirect $ fromJust f


-------------------------------------------------------------------------------
-- |
backSplice :: MonadSnap m => HeistT m m Template
backSplice = do
  f <- rqURI `fmap` getRequest
  textSplice $ T.decodeUtf8 f

backCSplice :: C.Splice (Handler b v)
backCSplice = return $ C.yieldRuntime $ do
  lift $ (fromByteString . rqURI) `fmap` getRequest


-------------------------------------------------------------------------------
-- |
focusSplice :: SnapletLens (Snaplet v) (NavTrail b)
            -> Splice (Handler b v)
focusSplice lens = do
  uri <- lift $ with' lens getFocus
  maybe (return []) textSplice uri

focusCSplice :: SnapletLens (Snaplet v) (NavTrail b)
             -> C.Splice (Handler b v)
focusCSplice lens = return $ C.yieldRuntimeText $ do
  uri <- lift $ with' lens getFocus
  return $ fromMaybe "" uri


-------------------------------------------------------------------------------
-- |
addNavTrailSplices :: Snaplet (Heist b) -> Initializer b (NavTrail b) ()
addNavTrailSplices heist = do
  lens <- getLens
  addConfig heist $
    mempty { hcCompiledSplices = do
               "linkToFocus" ## focusCSplice lens
               "linkToBack" ## backCSplice
           , hcInterpretedSplices = do
               "linkToFocus" ## focusSplice lens
               "linkToBack" ## backSplice
           }


