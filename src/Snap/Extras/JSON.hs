{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Snap.Extras.JSON
    ( 
    -- * Parsing JSON from Request Body
      getBoundedJSON
    , getJSON
    , reqBoundedJSON
    , reqJSON
    , getJSONField
    , reqJSONField
    -- * Sending JSON Data
    , writeJSON
    ) where
    

-------------------------------------------------------------------------------
import           Data.Aeson            as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Int
import           Snap.Core
-------------------------------------------------------------------------------
import           Snap.Extras.CoreUtils
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body assuming it is not larger
-- than 50000 bytes.
reqJSON :: (MonadSnap m, A.FromJSON b) => m b
reqJSON = reqBoundedJSON 50000


-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body with a size up to N
-- bytes. If parsing fails for any reson, request is terminated early
-- and a server error is returned.
reqBoundedJSON 
    :: (MonadSnap m, FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m a
reqBoundedJSON n = do
  res <- getBoundedJSON n
  case res of
    Left e -> badReq $ B.pack e
    Right a -> return a


-------------------------------------------------------------------------------
-- | Try to parse request body as JSON with a default max size of
-- 50000.
getJSON :: (MonadSnap m, A.FromJSON a) => m (Either String a)
getJSON = getBoundedJSON 50000


-------------------------------------------------------------------------------
-- | Parse request body into JSON or return an error string.
getBoundedJSON 
    :: (MonadSnap m, FromJSON a) 
    => Int64 
    -- ^ Maximum size in bytes
    -> m (Either String a)
getBoundedJSON n = do
  bodyVal <- A.decode `fmap` readRequestBody n
  return $ case bodyVal of
    Nothing -> Left "Can't find JSON data in POST body"
    Just v -> case A.fromJSON v of
                A.Error e -> Left e
                A.Success a -> Right a


-------------------------------------------------------------------------------
-- | Get JSON data from the given Param field
getJSONField 
    :: (MonadSnap m, FromJSON a)
    => B.ByteString
    -> m (Either String a)
getJSONField fld = do
  val <- getParam fld
  return $ case val of
    Nothing -> Left $ "Cant find field " ++ B.unpack fld
    Just val' ->
      case A.decode (LB.fromChunks . return $ val') of
        Nothing -> Left $ "Can't decode JSON data in field " ++ B.unpack fld
        Just v -> 
          case A.fromJSON v of
            A.Error e -> Left e
            A.Success a -> Right a


-------------------------------------------------------------------------------
-- | Force the JSON value from field. Similar to 'getJSONField'
reqJSONField 
    :: (MonadSnap m, FromJSON a)
    => B.ByteString
    -> m a
reqJSONField fld = do
  res <- getJSONField fld
  case res of
    Left e -> badReq $ B.pack e
    Right a -> return a


-------------------------------------------------------------------------------
-- | Set MIME to 'application/json' and write given object into
-- 'Response' body.
writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  writeLBS . encode $ a