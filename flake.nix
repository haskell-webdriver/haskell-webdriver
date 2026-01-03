{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        pkgs = import nixpkgs { inherit system; };


      in
        {
          devShells = {
            default = pkgs.mkShell {
              buildInputs = with pkgs; [
                pkg-config
                zlib

                haskell.compiler.ghc9122
              ];
            };
          };
        }
    );
}
