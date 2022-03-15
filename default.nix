# CAUTION! a spelling mistake in arg string is ignored silently.
#
# To use ghc-8.6.5
# nix-shell --argstr compiler "ghc865"

{
  nixpkgs ?
    import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz)
        {}
, compiler ? "default"
, c2nix ? "" # cabal2nix CLI options
# TODO
#, sources ? [] # e.g. [./. ./benchmark]
#, hdeps ? [] # e.g. [time, mtl]
#, deps ? [] # e.g. [SDL2]
}:
let haskellPackages =
        if compiler == "default"
        then nixpkgs.haskellPackages
        else nixpkgs.haskell.packages.${compiler};

    # we can possibly avoid adding our package to HaskellPackages like
    # in the case of nix-shell for a single package?
    mkPackage = super: pkg: path: opts: inShell:
                let orig = super.callCabal2nixWithOptions pkg path opts {};
                 in if inShell
                    # Avoid copying the source directory to nix store by using
                    # src = null.
                    then orig.overrideAttrs (oldAttrs: { src = null; })
                    else orig;

    flags = "--benchmark --flag fusion-plugin --flag doctests" + " " + c2nix;

    mkHaskellPackages = inShell:
        haskellPackages.override {
            # We could disbale doCheck on all like this, but it would make the
            # whole world rebuild, we can't use the binary cache
            #packageSetConfig = self: super: {
            #    mkDerivation = drv: super.mkDerivation (drv // {
            #        doCheck = false;
            #    });
            #};
            overrides = self: super:
                with nixpkgs.haskell.lib;
                {
                    streamly = mkPackage super "streamly" ./. flags inShell;
                    streamly-core =
                        mkPackage super "streamly-core" ./core flags inShell;
                    streamly-benchmarks =
                        mkPackage super "streamly-benchmarks"
                            ./benchmark flags inShell;
                    streamly-tests =
                        mkPackage super "streamly-tests"
                            ./test flags inShell;
                    streamly-docs =
                        mkPackage super "streamly-docs"
                            ./docs flags inShell;

                    #fusion-plugin =
                    #  super.callHackageDirect
                    #    { pkg = "fusion-plugin";
                    #      ver = "0.2.3";
                    #      sha256 = "073wbhdxj1sh5160blaihbzkkhabs8s71pqhag16lvmgbb7a3hla";
                    #    } {};

                    unicode-data =
                      super.callHackageDirect
                        { pkg = "unicode-data";
                          ver = "0.2.0";
                          sha256 = "14crb68g79yyw87fgh49z2fn4glqx0zr53v6mapihaxzkikhkkc3";
                        } {};

                    tasty-bench = super.tasty-bench_0_3_1;

                    #tasty-bench =
                    #  super.callHackageDirect
                    #    { pkg = "tasty-bench";
                    #      ver = "0.3.1";
                    #      sha256 = "0ixdxa7qwl4kck12gwvxg9kydhckjsi5drxcypr33jcdj771j7a4";
                    #    } {};

                    #tasty =
                    #  super.callHackageDirect
                    #    { pkg = "tasty";
                    #      ver = "1.4.1";
                    #      sha256 = "0g1280gcpcvjbmyk83jv3y9gs2z7fvmcagi9rfs8c9x036nvjq6c";
                    #    } {};

                    # Example to Use a different version of a package
                    #QuickCheck = self.QuickCheck_2_14;

                    # Example to disable tests if tests fail or take too long
                    # or to use different configure flags if needed
                    #
                    # XXX We need the ability to disable doCheck on all
                    # those packages that are being built locally and
                    # not fetched from the cache. Running tests could do
                    # nasty things to the machine e.g. some tests even
                    # listen for incoming connections on the network.
                    #selective =
                    #    super.selective.overrideAttrs (oldAttrs:
                    #      { doCheck = false;
                    #        configureFlags =
                    #          oldAttrs.configureFlags ++ ["--disable-tests"];
                    #      });
                };
        };

    drv = mkHaskellPackages true;

    shell = drv.shellFor {
        packages = p:
          [ p.streamly
            p.streamly-core
            p.streamly-benchmarks
            p.streamly-tests
            p.streamly-docs
          ];
        # some dependencies of hoogle fail to build with quickcheck-2.14
        # We should use hoogle as external tool instead of building it here
        # withHoogle = true;
        doBenchmark = true;
        # XXX On macOS cabal2nix does not seem to generate a dependency on
        # Cocoa framework.
        buildInputs =
            if builtins.currentSystem == "x86_64-darwin"
            then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
            else [];
        # Use a better prompt
        shellHook = ''
          export CABAL_DIR="$(pwd)/.cabal.nix"
          if test -n "$PS_SHELL"
          then
            export PS1="$PS_SHELL\[$bldred\](nix:streamly)\[$txtrst\] "
          fi
        '';
    };
in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).streamly
