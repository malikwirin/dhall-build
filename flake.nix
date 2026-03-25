{
  description = "dhall-build dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskell = pkgs.haskellPackages;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            (haskell.ghcWithPackages (ps: with ps; [
              cabal-install
              dhall
              nix-derivation
            ]))
            pkgs.nix
          ];
        };

        packages.default = haskell.callCabal2nix "dhall-build" ./. {};
      });
}

