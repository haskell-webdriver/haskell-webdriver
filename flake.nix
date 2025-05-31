{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
  inputs.nixpkgsMaster.url = "github:NixOS/nixpkgs/master";

  outputs = { self, flake-utils, nixpkgs, nixpkgsMaster }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgsMaster = import nixpkgsMaster { inherit system; };

      in
        {
          packages = {
            inherit (pkgsMaster.haskell.packages.ghc966) weeder;
          };

          devShells = {
            default = pkgs.mkShell {
              buildInputs = with pkgs; [
                gmp
                ncurses
                zlib
              ];
            };
          };
        }
    );
}
