{-# LANGUAGE OverloadedStrings #-}
module NormalizerSpec ( spec ) where

import Test.Hspec
import Data.Text ( Text )
import Dhall.Core ( Expr(..), Var(..) )
import Dhall.Parser ( Src )
import Data.Functor.Identity ( runIdentity )
import Data.IORef ( newIORef )
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.TypeCheck

import DhallBuild.DerivationTree ( mkReifiedNormalizer )

runNorm :: Expr Src Dhall.TypeCheck.X -> IO (Maybe (Expr Src Dhall.TypeCheck.X))
runNorm e = do
  ref <- newIORef []
  let Dhall.Core.ReifiedNormalizer norm = mkReifiedNormalizer ref
  return $ runIdentity (norm e)

parseExpr :: Text -> Expr Src Dhall.TypeCheck.X
parseExpr t = case Dhall.Parser.exprFromText mempty t of
  Right e -> fmap (\_ -> error "unexpected import") (Dhall.Core.normalize e)
  Left  e -> error ( show e )

var :: Text -> Expr Src Dhall.TypeCheck.X
var name = Var (V name 0)

spec :: Spec
spec = do
  describe "dhallBuildNormalizer" $ do

    it "does not fire on a bare variable" $ do
      result <- runNorm $ App (var "derivation") (var "x")
      result `shouldBe` Nothing

    it "does not fire on a non-derivation application" $ do
      result <- runNorm $ App (var "somethingElse") (var "arg")
      result `shouldBe` Nothing

    it "does not fire on a derivation call with unevaluated args" $ do
      result <- runNorm $ App (var "derivation") (var "unevaluated")
      result `shouldBe` Nothing

    it "fires when derivation is applied to a fully evaluated record" $ do
      let record = parseExpr
            "{ name = \"hello-script\"\
            \, system = < builtin | x86_64-linux >.x86_64-linux\
            \, builder = (< Builtin : < Fetch-Url : {} > | Exe : Text >).Exe \"/bin/sh\"\
            \, args = [\"-c\", \"echo hello\"]\
            \, environment = [] : List { name : Text, value : < Bool : Bool | Text : Text > }\
            \, outputs = [\"out\"]\
            \, output-hash = None { algorithm : < SHA256 >, hash : Text, mode : < Flat | Recursive > }\
            \}"
      result <- runNorm $ App (var "derivation") record
      result `shouldNotBe` Nothing

