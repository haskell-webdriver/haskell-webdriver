{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/master";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";

  outputs = { self, flake-utils, haskellNix, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [haskellNix.overlay];
          config = haskellNix.config;
        };

      in
        {
          packages = {
            weeder = pkgs.callPackage ./nix/weeder.nix {
              compiler-nix-name = "ghc9124";
            };
          };

          devShells = {
            default = pkgs.mkShell {
              buildInputs = (with pkgs; [
                gmp
                ncurses
                pkg-config
                zlib
              ]) ++ (with pkgs; [
                haskell.compiler.ghc9124
                # (haskell-language-server.override { supportedGhcVersions = ["9124"]; })
              ]);
            };
          };
        }
    );
}
