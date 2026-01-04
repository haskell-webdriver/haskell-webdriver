{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/master";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";
  inputs.nixpkgsMaster.url = "github:NixOS/nixpkgs/master";

  outputs = { self, flake-utils, haskellNix, nixpkgs, nixpkgsMaster }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgsMaster = import nixpkgsMaster {
          inherit system;
          overlays = [haskellNix.overlay];
          config = haskellNix.config;
        };

      in
        {
          packages = {
            weeder = pkgsMaster.callPackage ./nix/weeder.nix {
              compiler-nix-name = "ghc9102";
            };
          };

          devShells = {
            default = pkgs.mkShell {
              buildInputs = (with pkgs; [
                gmp
                ncurses
                pkg-config
                zlib
              ]) ++ (with pkgsMaster; [
                haskell.compiler.ghc9122
                (haskell-language-server.override { supportedGhcVersions = ["9122"]; })
              ]);
            };
          };
        }
    );
}
