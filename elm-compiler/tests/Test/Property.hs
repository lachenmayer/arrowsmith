module Test.Property where

import Control.Applicative ((<*))
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (assert)
import Test.QuickCheck
import Text.Parsec.Combinator (eof)
import Text.PrettyPrint as P

import AST.Literal as Lit
import qualified AST.Pattern as P
import qualified AST.Variable as V
import AST.PrettyPrint (Pretty, pretty)
import Parse.Helpers (IParser, iParse)
import Parse.Literal (literal)
import qualified Parse.Pattern as Pat (expr)
import qualified Parse.Type as Type (expr)
import Test.Property.Arbitrary


propertyTests :: Test
propertyTests =
  testGroup "Parse/Print Agreement Tests"
  [ testCase "Long Pattern test" $ assert (prop_parse_print Pat.expr longPat)
  , testProperty "Literal test" $ prop_parse_print literal
  , testProperty "Pattern test" $ prop_parse_print Pat.expr
  , testProperty "Type test" $ prop_parse_print Type.expr
  ]

  where
    -- This test was autogenerated from the Pattern test and should be
    -- left in all its ugly glory.
    longPat = P.Data (V.Raw "I")
              [ P.Literal (Lit.Chr '+')
              , P.Record
                [ "q7yclkcm7k_ikstrczv_"
                , "wQRv6gKsvvkjw4b5F"
                ,"c9'eFfhk9FTvsMnwF_D"
                ,"yqxhEkHvRFwZ"
                ,"o"
                ,"nbUlCn3y3NnkVoxhW"
                ,"iJ0MNy3KZ_lrs"
                ,"ug"
                ,"sHHsX"
                ,"mRKs9d"
                ,"o2KiCX5'ZRzHJfRi8" ]
              , P.Var "su'BrrbPUK6I33Eq"
              ]


prop_parse_print :: (Pretty a, Arbitrary a, Eq a) => IParser a -> a -> Bool
prop_parse_print parser value =
  either (const False) (== value) (parse_print parser value)


parse_print :: (Pretty a) => IParser a -> a -> Either String a
parse_print parser value =
  let doc =
        pretty value

      string =
        P.renderStyle P.style { mode = P.LeftMode } doc
  in
      case iParse (parser <* eof) string of
        Left msg -> Left (show msg)
        Right x -> Right x 
