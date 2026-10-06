# Shell for building with the GHC JavaScript backend, use "nix develop .#js".
# Provides javascript-unknown-ghcjs-ghc, its ghc-pkg and hsc2hs, with the
# non-boot dependencies of streamly-core, streamly and streamly-tests
# installed, and node to run the generated code. Use with cabal.project.ghcjs.
{ nixpkgs }:
let
  # nixpkgs enables iserv-proxy for cross compiled Template Haskell. It does
  # not build for JS because its network dependency fails, and the JS
  # backend runs Template Haskell itself using node.
  jsHaskellPackages =
    nixpkgs.pkgsCross.ghcjs.haskell.packages.ghc910.override {
      overrides = self: super: {
        mkDerivation = args:
          super.mkDerivation (args // { enableExternalInterpreter = false; });
      };
    };

  ghc = jsHaskellPackages.ghcWithPackages (p: with p; [
    # streamly-core
    fusion-plugin-types
    heaps
    monad-control

    # streamly, atomic-primops, lockfree-queue and network do not build for JS
    hashable
    unicode-data
    unordered-containers

    # streamly-tests, network does not build for JS
    QuickCheck
    hspec
    random
    scientific
    temporary
  ]);

  # When cabal reuses a build directory it looks up ghc-pkg by its plain name,
  # ignoring with-hc-pkg, and can pick up a native ghc-pkg from PATH. Provide
  # the plain names for the JS tools so that they are found first.
  ghcAliases = nixpkgs.runCommand "ghcjs-aliases" {} ''
    mkdir -p $out/bin
    install -m 755 ${./bin/ghcjs-ghc} $out/bin/ghcjs-ghc
    for t in ghc ghc-pkg hsc2hs; do
      ln -s ${ghc}/bin/javascript-unknown-ghcjs-$t $out/bin/$t
    done
  '';
in
nixpkgs.mkShell {
  packages = [ ghcAliases ghc nixpkgs.nodejs nixpkgs.cabal-install ];
  shellHook = ''
    # Use an empty cabal config, all dependencies come from nix
    export CABAL_CONFIG=/dev/null
  '';
}
