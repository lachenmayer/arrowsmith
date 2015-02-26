{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module Snap.Extras.FormUtils
    (
    -- * Transformers
      maybeTrans
    , readMayTrans
    , readTrans
    -- * Compiled splices
    , editFormSplice

    -- -- * Form Elements
    -- , submitCancel
    -- , submitOnly
    -- -- * Formatting Form Elements
    -- , inputFormat
    -- , dateInput
    -- -- * Running/Displaying Forms
    -- , showForm
    -- , runViewForm'
    ) where

-------------------------------------------------------------------------------
import           Control.Error
import qualified Data.ByteString.Char8 as B
import           Data.String
import           Data.Text             (Text)
import           Data.Text.Encoding
import qualified Data.Text             as T
import           Heist
import           Snap.Core
import           Text.Digestive
import qualified Text.XmlHtml           as X
-------------------------------------------------------------------------------


                              ------------------
                              -- Transformers --
                              ------------------


-------------------------------------------------------------------------------
-- | Transform to Nothing if field is empty string
maybeTrans
    :: (Eq a, IsString a)
    => a -> Result v (Maybe a)
maybeTrans x = f x
    where f "" = Success Nothing
          f a = Success $ Just a


-------------------------------------------------------------------------------
-- | Maybe read into target value
readMayTrans
    :: Read a
    => Text -> Result v (Maybe a)
readMayTrans x = Success $ readMay (T.unpack x)


-------------------------------------------------------------------------------
-- | Read into target value
readTrans
    :: (Read a, IsString v)
    => Text -> Result v a
readTrans a = maybe (Error "Unrecognized input") Success $ readMay . T.unpack $ a



                              -------------------
                              -- Form Elements --
                              -------------------


-- -------------------------------------------------------------------------------
-- -- | Submit and cancel buttons with markup
-- submitCancel
--     :: (Monad m, Functor m)
--     => String
--     -- ^ Submit button text
--     -> Text
--     -- ^ Cancel button text
--     -> Text
--     -- ^ Cancel button HREF
--     -> Form m i e (FormHtml Html) ()
-- submitCancel s ct cl = mapViewHtml enc (submit s)
--   where enc x = do
--           H.div ! A.class_ "form-actions" $ do
--             x
--             " "
--             H.a (text ct) ! A.href (textValue cl) ! A.class_ "btn"

-- -------------------------------------------------------------------------------
-- -- | Just a submit button with markup
-- submitOnly
--     :: (Monad m, Functor m)
--     => String
--     -- ^ Submit button text
--     -> Form m i e (FormHtml Html) ()
-- submitOnly s = mapViewHtml enc (submit s)
--   where enc x = do
--           H.div ! A.class_ "form-actions" $ x


-- -------------------------------------------------------------------------------
-- -- | Input form element wrapper to add a class of 'input'
-- inputFormat
--     :: (Monad m, Functor m)
--     => Form m i e (FormHtml Html) a
--     -> Form m i e (FormHtml Html) a
-- inputFormat = mapViewHtml (H.div ! A.class_ "input")


-- -------------------------------------------------------------------------------
-- -- | Add a 'datepicker' classed div tag surrounding the form element
-- dateInput
--     :: (Monad m, Functor m)
--     => Form m i e (FormHtml Html) a
--     -> Form m i e (FormHtml Html) a
-- dateInput = mapViewHtml (H.div ! A.class_ "datepicker")


-- -------------------------------------------------------------------------------
-- -- | Render a form into a Heist splice
-- showForm
--     :: Monad m
--     => AttributeValue
--     -- ^ Form action target
--     -> AttributeValue
--     -- ^ Form submittion HTTP method
--     -> FormHtml (HtmlM a)
--     -- ^ Form to display
--     -> Splice m
-- showForm act mth frm =
--    let  (formHtml', enctype) = renderFormHtmlWith config frm
--         config = defaultHtmlConfig { htmlSubmitClasses = ["btn", "btn-primary"] }
--         frm' = H.form ! A.enctype (H.toValue $ show enctype) ! A.method mth
--                       ! A.action act $ formHtml' >> return ()
--    in return $ renderHtmlNodes frm'



-- -------------------------------------------------------------------------------
-- -- | Version of runView that will use defaults when no related
-- -- parameters are found in the environment. This is particularly
-- -- designed for filter forms that have to be rendered all the time on
-- -- the page (as opposed to record create/update forms).
-- --
-- -- This is a pretty smart combinator and will try to do the "Right
-- -- Thing" automagically. It will:
-- --
-- -- 1. Look for the presence of a form namespace in Snap's 'Params'
-- --
-- -- 2. If it's there, it'll try to run the form and the views
-- --
-- -- 3. If no params are there, it'll assume that the form has not been
-- -- submitted, so it should be a default form. It just renders the view
-- -- in its default mode.
-- runViewForm'
--   :: MonadSnap m
--   => SnapForm m e t a
--   -- ^ A digestive-functor form
--   -> String
--   -- ^ Namespace used when rendering the form
--   -> m (t, Maybe a)
--   -- ^ The form's view and a possible form result
-- runViewForm' form nm = do
--   ps <- M.keys `fmap` getParams
--   case find (B.isInfixOf $ B.pack nm) ps of
--     Just _ -> runViewSnapForm form nm
--     Nothing -> do
--       view' <- viewForm form nm
--       return $ (view', Nothing)

-------------------------------------------------------------------------------
-- | Constructs a generalized edit form splice that looks up an ID param
-- specified by the @by@ attribute.  You might use this splice as follows:
--
-- > <editFormSplice by="id">
--
-- If you don't specify the @by@ attribute, the default is @by=\"id\"@.
editFormSplice :: (Monad m, MonadSnap n)
               => (n (Maybe a) -> HeistT n m b)
               -- ^ Function for generating a splice from an optional default
               -- value calculated at runtime.
               -> (B.ByteString -> n (Maybe a))
               -- ^ Function for retrieving the form default by ID.
               -> HeistT n m b
editFormSplice formSplice getById = do
    node <- getParamNode
    let param = fromMaybe "id" $ X.getAttribute "by" node
    formSplice $ do
      runMaybeT $ do
        key <- MaybeT $ getParam $ encodeUtf8 param
        MaybeT (getById key)


