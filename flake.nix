{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      hPkgs = pkgs.haskell.packages."ghc966";
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          # project
          hPkgs.ghc
          hPkgs.cabal-install
          hPkgs.haskell-language-server

          pkgs.zlib
          pkgs.pkg-config # need to find zlib
        ];

        shellHook = ''
          echo "🚀 You're in the toorip-pay dev shell"
        '';
      };
    });
}
