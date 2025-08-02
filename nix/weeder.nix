{ compiler-nix-name, pkgs, ... }:

let
  weederSrc = pkgs.fetchFromGitHub {
    owner = "mmhat";
    repo = "weeder";
    rev = "034e1a84c11061858e8010e35ffdff122b049585";
    sha256 = "sha256-Zwa01QDGGTnuETReTH9BQ8DKeWc0mcyW5cprrjMX1VM=";
  };

in

(pkgs.haskell-nix.project {
  inherit compiler-nix-name;

  cabalProjectLocal = builtins.readFile "${weederSrc}/cabal.project.haskell-nix";

  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "weeder";
    src = weederSrc;
  };

  modules = [
    {
      reinstallableLibGhc = false;
      enableLibraryProfiling = true;
      nonReinstallablePkgs = [
        "rts" "ghc-prim" "integer-gmp" "integer-simple" "base" "terminfo"
        "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
        "ghc-bignum" "system-cxx-std-lib" "ghc" "binary" "bytestring" "containers"
        "directory" "exceptions" "filepath" "hpc" "process" "semaphore-compat" "stm"
        "time" "transformers" "unix" "mtl"
      ];
    }
  ];
}).weeder.components.exes.weeder
