{ nixpkgsPath ? <nixpkgs>, compiler ? "default", doBenchmark ? false }:

let

  inherit (import nixpkgsPath {
    overlays = [
      (self: super: rec {
        fllvm = super.llvm.overrideAttrs (_: rec {
          version = "7.1.0";
        });
        tscope = super.haskellPackages.threadscope;
      })
    ];
  }) pkgs;
  f = { mkDerivation, array, base, deepseq, directory
      , numeric-limits, parallel, pretty-terminal, random, stdenv, time
      , fllvm, tscope
      }:
      mkDerivation {
        pname = "ray-tracers";
        version = "0.2.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          array base deepseq directory numeric-limits parallel
          pretty-terminal random time
        ];
        librarySystemDepends = [
          fllvm
        ];
        executableSystemDepends = [
          tscope
        ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
