{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Dhall.Core ( Expr(..) )
import qualified Dhall.Map as Map
import Control.Monad.Trans.State.Strict ( evalStateT )
import Data.IORef ( newIORef, readIORef )
import Lens.Family
import Control.Applicative ( (<**>) )
import Control.Exception ( throwIO )
import qualified Dhall.Import

import qualified Data.Text.IO as Text
import qualified Dhall.Context
import qualified Dhall.Map
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Options.Applicative as OptParse

import qualified DhallBuild.DerivationTree as DhallBuild
import qualified Nix.Daemon


commandLineParser :: OptParse.ParserInfo FilePath
commandLineParser =
  OptParse.info ( parser <**> OptParse.helper ) mempty
  where
  parser = OptParse.strArgument ( OptParse.metavar "FILE" )


main :: IO ()
main = do
  f <- OptParse.execParser commandLineParser
  t <- Text.readFile f

  parsedExpr <-
    case Dhall.Parser.exprFromText mempty t of
      Left e  -> throwIO e
      Right a -> return a

  ref <- newIORef []

  res <-
    evalStateT
      ( Dhall.Import.loadWith parsedExpr )
      ( Dhall.Import.emptyStatus "."
          & Dhall.Import.normalizer .~ Just (DhallBuild.mkReifiedNormalizer ref)
          & Dhall.Import.startingContext .~ context
      )

  derivationTrees <- readIORef ref

  mapM_ print derivationTrees

  Nix.Daemon.withDaemon $ \nixDaemon ->
    mapM_ ( DhallBuild.addDerivationTree nixDaemon ) derivationTrees

  Text.putStrLn ( Dhall.Core.pretty res )


mf :: Expr s a -> Dhall.Core.RecordField s a
mf = Dhall.Core.makeRecordField


context =
  Dhall.Context.insert
    "derivation"
    ( Pi
        Nothing
        "_"
        ( Record
            ( Map.fromList
                [ ( "args", mf ( List `App` Text ) )
                , ( "builder"
                  , mf ( Union
                      ( Map.fromList
                          [ ( "Builtin"
                            , Just ( Union
                                ( Map.fromList
                                    [ ( "Fetch-Url", Nothing ) ]
                                ) )
                            )
                          , ( "Exe", Just Text )
                          ]
                      ) )
                  )
                , ( "environment"
                  , mf ( List
                      `App`
                        Record
                          ( Map.fromList
                              [ ( "name", mf Text )
                              , ( "value"
                                , mf ( Union
                                    ( Map.fromList
                                        [ ( "Bool", Just Bool )
                                        , ( "Text", Just Text )
                                        ]
                                    ) )
                                )
                              ]
                          ) )
                  )
                , ( "name", mf Text )
                , ( "output-hash"
                  , mf ( Optional
                      `App`
                        Record
                          ( Map.fromList
                              [ ( "algorithm"
                                , mf ( Union
                                    ( Map.fromList
                                        [ ( "SHA256", Nothing ) ]
                                    ) )
                                )
                              , ( "hash", mf Text )
                              , ( "mode"
                                , mf ( Union
                                    ( Map.fromList
                                        [ ( "Flat", Nothing )
                                        , ( "Recursive", Nothing )
                                        ]
                                    ) )
                                )
                              ]
                          ) )
                  )
                , ( "outputs", mf ( List `App` Text ) )
                , ( "system"
                  , mf ( Union
                      ( Map.fromList
                          [ ( "builtin", Nothing )
                          , ( "x86_64-linux", Nothing )
                          ]
                      ) )
                  )
                ]
            )
        )
        Text
    )
    Dhall.Context.empty

