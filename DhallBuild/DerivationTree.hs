{-# LANGUAGE OverloadedStrings #-}

module DhallBuild.DerivationTree
  ( DerivationTree(..)
  , mkReifiedNormalizer
  , addDerivationTree
  ) where

import Data.Foldable (fold, toList)
import Data.Bifunctor
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State.Class ( modify )
import Control.Monad.Trans.State.Strict ( runStateT )
import Crypto.Hash ( SHA256, hashlazy )
import Data.Function ( (&) )
import Data.Functor.Identity ( Identity(..) )
import Data.IORef ( IORef, modifyIORef )
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )
import Data.Text.Lazy ( Text )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Control.Monad.Trans.State.Strict ( StateT )
import System.IO.Unsafe ( unsafePerformIO )

import qualified Dhall.Map as InsOrdMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyBuilder
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Core as Expr ( Expr(..), Chunks(..) )
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck

import qualified MemoIO
import qualified Nix.Daemon
import qualified Nix.Derivation
import qualified Nix.Derivations as Nix.Derivations
import qualified Nix.Instantiate
import qualified Nix.StorePath


-- | Unwrap RecordField and return the inner Expr
lookupExpr
  :: Dhall.Text
  -> InsOrdMap.Map Dhall.Text (Dhall.Core.RecordField s a)
  -> Maybe (Expr.Expr s a)
lookupExpr key m = Dhall.Core.recordFieldValue <$> InsOrdMap.lookup key m

-- | Extract a Haskell value from a Dhall Expr, returning Nothing on failure
extractMaybe
  :: Dhall.FromDhall a
  => Expr.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  -> Maybe a
extractMaybe = either (const Nothing) Just . Dhall.toMonadic . Dhall.extract Dhall.auto

-- | Extract the union tag name and payload from a union application
unionTag :: Expr.Expr s a -> Maybe (Dhall.Text, Expr.Expr s a)
unionTag (Expr.App (Expr.Field (Expr.Union _) fs) val) =
  Just (Dhall.Core.fieldSelectionLabel fs, val)
unionTag _ = Nothing

-- | Wrap dhallBuildNormalizer into a ReifiedNormalizer using IORef for state
mkReifiedNormalizer
  :: IORef [DerivationTree]
  -> Dhall.Core.ReifiedNormalizer Dhall.TypeCheck.X
mkReifiedNormalizer ref = Dhall.Core.ReifiedNormalizer $ \e ->
  Identity $ unsafePerformIO $ do
    (result, trees) <- runStateT (dhallBuildNormalizer e) []
    modifyIORef ref (trees ++)
    return result

data DerivationTree
  = DerivationTree
      { dtInputs :: [DerivationTree]
      , dtExec :: Data.Text.Text
      , dtArgs :: Dhall.Vector Data.Text.Text
      , dtName :: Text
      , dtEnv :: Map.Map Data.Text.Text Data.Text.Text
      , dtSystem :: Data.Text.Text
      }
  | EvalNix Text
  deriving (Show)


dhallBuildNormalizer
  :: Expr.Expr s Dhall.TypeCheck.X
  -> StateT [ DerivationTree ] IO ( Maybe ( Expr.Expr s Dhall.TypeCheck.X ) )
dhallBuildNormalizer e = do
  liftIO $ putStrLn ( show ( Dhall.Pretty.prettyExpr e ) )

  case e of
    Expr.App ( Expr.Var "derivation" ) args | not ( Dhall.Core.freeIn "args" args ) ->
      Just <$> derivation args

    _ ->
      return Nothing


derivation :: Expr.Expr s Dhall.TypeCheck.X -> StateT [ DerivationTree ] IO ( Expr.Expr s Dhall.TypeCheck.X )
derivation args = do
  ( e, inputs ) <-
    liftIO ( runStateT ( Dhall.Core.normalizeWithM dhallBuildNormalizer args ) [] )

  Expr.RecordLit fields' <- return e

  liftIO $ putStrLn ( show ( Dhall.Pretty.prettyExpr args ) )

  let
    Just builder =
      case lookupExpr "builder" fields' >>= unionTag of
          Just ("Builtin", inner) ->
              case unionTag inner of
                  Just ("Fetch-Url", _) -> Just "builtin:fetchurl"
                  _                     -> Nothing
          Just ("Exe", str) -> extractMaybe str
          _                 -> Nothing

    env =
      case lookupExpr "environment" fields' of
        Just (Expr.ListLit _ xs) ->
          flip map (toList xs) $ \(Expr.RecordLit x) ->
            let
              Just name =
                case lookupExpr "name" x of
                  Just t  -> extractMaybe t
                  Nothing -> Nothing
              Just value =
                case lookupExpr "value" x >>= unionTag of
                  Just ("Bool", Expr.BoolLit True) -> Just "1"
                  Just ("Bool", _)                 -> Just "0"
                  Just ("Text", t)                 -> extractMaybe t
                  _                                -> Nothing
            in (name, value)
        _ -> []

    outputHashBindings (Expr.RecordLit hashArgs) =
        let
          mode =
            case lookupExpr "mode" hashArgs >>= unionTag of
                Just ("Flat", _)      -> "flat"
                Just ("Recursive", _) -> "recursive"
                _                     -> ""
          Just hash = lookupExpr "hash" hashArgs >>= extractMaybe
          algorithm =
            case lookupExpr "algorithm" hashArgs >>= unionTag of
                Just ("SHA256", _) -> "sha256"
                _                  -> ""
        in Map.fromList
             [("outputHashMode", mode)
             ,("outputHash", hash)
             ,("outputHashAlgo", algorithm)
             ]

    moutputHash =
      case lookupExpr "output-hash" fields' of
          Just (Expr.Some e) -> Just ( outputHashBindings e )
          Just _             -> Nothing
          Nothing            -> Nothing

    system =
      case lookupExpr "system" fields' >>= unionTag of
          Just ("builtin", _)      -> "builtin"
          Just ("x86_64-linux", _) -> "x86_64-linux"
          _                        -> ""

  let
    this =
      DerivationTree
        { dtInputs = inputs
        , dtExec = builder
        , dtSystem = system
        , dtArgs =
            fromMaybe
              ( error "args missing" )
              ( lookupExpr "args" fields' >>= extractMaybe )
        , dtName =
            fromMaybe
              ( error "Name missing" )
              ( lookupExpr "name" fields' >>= extractMaybe )
        , dtEnv = Map.fromList env <> fold moutputHash
        }

  modify (this : )

  d <- derivationTreeToDerivation this

  return
    ( Nix.Derivation.outputs d
        & Map.lookup "out"
        & fromMaybe ( error "No output" )
        & Nix.Derivation.path
        & fromString
        & Expr.TextLit
    )


addDerivationTree
  :: ( MonadIO m )
  => Nix.Daemon.NixDaemon -> DerivationTree -> m ()
addDerivationTree daemon t@DerivationTree{} = do
  mapM_ ( addDerivationTree daemon ) ( dtInputs t )

  drv <- derivationTreeToDerivation t

  let src = LazyBuilder.toLazyText ( Nix.Derivation.buildDerivation drv )

  liftIO $ do
    added <-
      Nix.Daemon.addTextToStore daemon ( dtName t <> ".drv" ) src []
    putStrLn $ "Added " <> LazyText.unpack added

addDerivationTree _ (EvalNix _) =
  return ()


derivationTreeToDerivation
  :: ( MonadIO m )
  => DerivationTree -> m (Nix.Derivation.Derivation FilePath Data.Text.Text)
derivationTreeToDerivation = \case
  EvalNix src -> do
    drvPath <- liftIO ( Nix.Instantiate.instantiateExpr src )
    liftIO ( Nix.Derivations.loadDerivation drvPath )

  t@DerivationTree{} -> do
    maskedInputs <-
      fmap Map.fromList
        ( mapM
            ( \t -> do
                d <- derivationTreeToDerivation t
                hash <- hashDerivationModulo d
                return ( fromString hash, Set.singleton "out" )
            )
            ( dtInputs t )
        )
    actualInputs <-
      fmap Map.fromList
        ( mapM
            ( \t -> do
                d <- derivationTreeToDerivation t
                path <- case t of
                  EvalNix src ->
                    liftIO ( fromString <$> Nix.Instantiate.instantiateExpr src )
                  DerivationTree{} -> return $
                    fromString $ Nix.StorePath.textPath
                      (dtName t <> ".drv")
                      (LazyBuilder.toLazyText (Nix.Derivation.buildDerivation d))
                return ( path, Set.singleton "out" )
            )
            ( dtInputs t )
        )

    let
      drv =
        Nix.Derivation.Derivation
          { Nix.Derivation.outputs =
              Map.singleton
                "out"
                (Nix.Derivation.DerivationOutput
                   (fromString (Nix.StorePath.derivationOutputPath (dtName t) drv
                               { Nix.Derivation.inputDrvs = maskedInputs }))
                   ""
                   "")
          , Nix.Derivation.inputDrvs = actualInputs
          , Nix.Derivation.inputSrcs = mempty
          , Nix.Derivation.platform = dtSystem t
          , Nix.Derivation.builder = dtExec t
          , Nix.Derivation.args = dtArgs t
          , Nix.Derivation.env =
              fmap
                ( fromString . Nix.Derivation.path )
                ( Nix.Derivation.outputs drv ) <>
              dtEnv t
          }

    return drv


hashDerivationFileModulo =
  go
  where
  go =
    MemoIO.memoIO $ \path -> do
      d <- liftIO ( Nix.Derivations.loadDerivation path )
      hashDerivationModulo d


hashDerivationModulo
  :: ( MonadIO m )
  => Nix.Derivation.Derivation FilePath Data.Text.Text -> m String
hashDerivationModulo =
  go
  where
  go derivation = do
    case Map.toList ( Nix.Derivation.outputs derivation ) of
      [ ( "out", Nix.Derivation.DerivationOutput path hashAlgo hash ) ] | not ( Data.Text.null hash ) ->
        return . show . hashlazy @SHA256 . encodeUtf8 . LazyText.fromStrict $
        "fixed:out:" <> hashAlgo <> ":" <> hash <> ":" <> Data.Text.pack path

      _ -> do
        maskedInputs <-
          fmap Map.fromList
            ( mapM
                ( \(path, outs) -> do
                    hash <- liftIO ( hashDerivationFileModulo path )
                    return ( fromString hash, outs )
                )
                ( Map.toList ( Nix.Derivation.inputDrvs derivation ) )
            )
        return $
          show . hashlazy @SHA256 . encodeUtf8 . LazyBuilder.toLazyText $
          Nix.Derivation.buildDerivation derivation { Nix.Derivation.inputDrvs = maskedInputs }

