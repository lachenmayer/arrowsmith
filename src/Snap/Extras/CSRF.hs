{-# LANGUAGE OverloadedStrings #-}

module Snap.Extras.CSRF where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as B
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as T
import           Snap
import           Snap.Snaplet.Session
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml          as X
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- | A splice that makes the CSRF token available to templates.  Typically we
-- use it by binding a splice and using the CSRF token provided by the session
-- snaplet as follows:
--
-- @(\"csrfToken\", csrfTokenSplice $ with session 'csrfToken')@
--
-- Where @session@ is a lens to the session snaplet.  Then you can make it
-- available to javascript code by putting a meta tag at the top of every
-- page like this:
--
-- > <meta name="csrf-token" content="${csrfToken}">
csrfTokenSplice :: Monad m
                => m Text
                -- ^ A computation in the runtime monad that gets the
                -- CSRF protection token.
                -> Splice m
csrfTokenSplice f = do
    token <- lift f
    textSplice token


------------------------------------------------------------------------------
-- | Adds a hidden _csrf input field as the first child of the bound tag.  For
-- full site protection against CSRF, you should bind this splice to the form
-- tag, and then make sure your app checks all POST requests for the presence
-- of this CSRF token and that the token is randomly generated and secure on a
-- per session basis.
secureForm :: MonadIO m
           => m Text
           -- ^ A computation in the runtime monad that gets the CSRF
           -- protection token.
           -> Splice m
secureForm mToken = do
    n <- getParamNode
    token <- lift mToken
    let input = X.Element "input"
          [("type", "hidden"), ("name", "_csrf"), ("value", token)] []
    case n of
      X.Element nm as cs -> do
        cs' <- runNodeList cs
        let newCs = if take 1 cs' == [input] then cs' else (input : cs')
        stopRecursion
        return [X.Element nm as newCs]
      _ -> return [n] -- "impossible"


------------------------------------------------------------------------------
-- | Use this function to wrap your whole site with CSRF protection.  Due to
-- security considerations, the way Snap parses file uploads
-- means that the CSRF token cannot be checked before the file uploads have
-- been handled.  This function protects your whole site except for handlers
-- of multipart/form-data forms (forms with file uploads).  To protect those
-- handlers, you have to call handleCSRF explicitly after the file has been
-- processed.
blanketCSRF :: SnapletLens v SessionManager
            -- ^ Lens to the session snaplet
            -> Handler b v ()
            -- ^ Handler to run if the CSRF check fails
            -> Handler b v ()
blanketCSRF session onFailure = do
    h <- getHeader "Content-type" `fmap` getRequest
    case maybe False (B.isInfixOf "multipart/form-data") h of
      True -> return ()
      False -> handleCSRF session onFailure


------------------------------------------------------------------------------
-- | If a request is a POST, check the CSRF token and fail with the specified
-- handler if the check fails.  If if the token is correct or if it's not a
-- POST request, then control passes through as a no-op.
handleCSRF :: SnapletLens v SessionManager
           -- ^ Lens to the session snaplet
           -> Handler b v ()
           -- ^ Handler to run on failure
           -> Handler b v ()
handleCSRF session onFailure = do
    m <- getsRequest rqMethod
    if m /= POST
      then return ()
      else do tok <- getParam "_csrf"
              realTok <- with session csrfToken
              if tok == Just (T.encodeUtf8 realTok)
                then return ()
                else onFailure >> getResponse >>= finishWith

