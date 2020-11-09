# CAUTION! a spelling mistake in arg string is ignored silently.
#
# To include benchmark deps use:
# nix-shell --argstr c2nix "--benchmark --flag fusion-plugin"
#
# To build the chart executable for running bench.sh use:
# nix-shell --argstr c2nix "--flag dev" --run "cabal build chart --flag dev"
#
# To use ghc-8.6.5
# nix-shell --argstr compiler "ghc865"

{
  nixpkgs ?
    import (builtins.fetchTarball https://github.com/composewell/nixpkgs/archive/01dd2b4e738.tar.gz)
        # fusion-plugin is marked as broken
        { config.allowBroken = true;}
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
                    streamly = mkPackage super "streamly" ./. c2nix inShell;
                    streamly-benchmarks =
                        mkPackage super "streamly-benchmarks"
                            ./benchmark c2nix inShell;

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
            p.streamly-benchmarks
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
          if test -n "$PS_SHELL"
          then
            export PS1="$PS_SHELL\[$bldred\](nix)\[$txtrst\] "
          fi
        '';
    };
in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).streamly
