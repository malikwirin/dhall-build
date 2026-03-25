module Nix.Derivations ( loadDerivation ) where

import qualified Data.Attoparsec.Text.Lazy as Attoparsec
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text as Text
import qualified Nix.Derivation as Nix

import qualified MemoIO


loadDerivation :: FilePath -> IO (Nix.Derivation FilePath Text.Text)
loadDerivation =
  MemoIO.memoIO go

  where

  go drvPath = do
    drvText <-
      LazyText.readFile drvPath

    let
      Attoparsec.Done _ result =
        Attoparsec.parse Nix.parseDerivation drvText

    return result
