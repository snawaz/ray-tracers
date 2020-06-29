# https://maybevoid.com/posts/2019-01-27-getting-started-haskell-nix.html
# https://www.reddit.com/r/haskell/comments/2yf1yc/how_to_install_ghc_and_cabal_under_nixos/
# https://dev.to/dannypsnl/nix-report-f8a

{
  source ? import ./nix/sources.nix,
  system ? builtins.currentSystem,
  overlays ? [ ],
  crossSystem ? null, # (import source.nixpkgs {}).lib.systems.examples.musl64,
}:
let
  inherit (source) nixpkgs all-hies;
  pkgs = import nixpkgs {
    # inherit system crossSystem overlays;
  };
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    #base
    lens
    #mtl
    #network
    #zlib
    stylish-haskell
  ];

  # ghc = (haskellPackages.ghcWithPackages haskellDeps) // haskellPackages.ghcWithHoogle;
  # ghc = haskellPackages.ghcWithHoogle haskellDeps;

  hie = (import all-hies {}).selection { selector = p: { inherit (p) ghc865; }; }; # GHC version match

  # nixPackages = [
  #   hie
  #   pkgs.gdb
  #   pkgs.nodejs
  #   haskellPackages.cabal-install
  #   pkgs.cabal2nix
  #   pkgs.zlib
  #   pkgs.openssl
  #   pkgs.icu
  #   pkgs.neovim
  #   # pkgs.crypto
  # ];
in
haskellPackages.shellFor {
  packages = ps: with ps; [ 
    lens 
    yesod
    gitlib
    hlibgit2
    gitlib-libgit2
    cabal-install 
    stylish-haskell
    haskell-formatter
  ];
  withHoogle = true;
  buildInputs = [ 
    pkgs.cabal-install
    pkgs.icu
    pkgs.openssl
    pkgs.zlib # required on fish
    hie
    haskellPackages.stylish-haskell
  ]; 
  shellHook = ''
    # fix: can't load .so/.DLL for: libicuuc.dylib (dlopen(libicuuc.dylib, 5): image not found)
    # also coc.vim does not work as it relies on hie: https://gitlab.haskell.org/ghc/ghc/issues/9868
    # set DYLD_LIBRARY_PATH $DYLD_LIBRARY_PATH ${pkgs.icu}
    export DYLD_LIBRARY_PATH=${pkgs.icu}/lib:$DYLD_LIBRARY_PATH
    # export HIE_HOOGLE_DATABASE=./hoogle-db
    echo "DYLD_LIBRARY_PATH is set to ${pkgs.icu}"
    #    taken from https://github.com/domenkozar/hie-nix
    # see this too: https://github.com/domenkozar/hie-nix/issues/27
    # HIE_HOOGLE_DATABASE = "/nix/store/vx6hv1pgr3szikq5sbv3sddzwm1m2yvw-hoogle-local-0.1/share/doc/hoogle/default.hoo";
    # export HIE_HOOGLE_DATABASE=$(cat $(which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')
    export HIE_HOOGLE_DATABASE="/nix/store/vx6hv1pgr3szikq5sbv3sddzwm1m2yvw-hoogle-local-0.1/share/doc/hoogle/default.hoo";
  '';
}
#
#(pkgs.mkShell {
#  name = "env";
#  inputsFrom = nixPackages;
#  nativeBuildInputs = nixPackages;
#  buildInputs = nixPackages;
#  shellHook = ''
#    # fix: can't load .so/.DLL for: libicuuc.dylib (dlopen(libicuuc.dylib, 5): image not found)
#    # set DYLD_LIBRARY_PATH $DYLD_LIBRARY_PATH ${pkgs.icu}
#    export DYLD_LIBRARY_PATH=${pkgs.icu}/lib:$DYLD_LIBRARY_PATH
#    # export HIE_HOOGLE_DATABASE=./hoogle-db
#    echo "DYLD_LIBRARY_PATH is set to ${pkgs.icu}"
#  '';
#}).overrideAttrs(oldAttrs: rec {
#  # same as: cat (which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p'
#  #    taken from https://github.com/domenkozar/hie-nix
#  # see this too: https://github.com/domenkozar/hie-nix/issues/27
#  HIE_HOOGLE_DATABASE = "/nix/store/vx6hv1pgr3szikq5sbv3sddzwm1m2yvw-hoogle-local-0.1/share/doc/hoogle/default.hoo";
#})
