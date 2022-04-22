let
  nixpkgsPath_21_11 =
    "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz";
  nixpkgsDefault = import (builtins.fetchTarball nixpkgsPath_21_11) { };
in
{
  nixpkgs ? nixpkgsDefault
, compiler ? "default"
}:
let haskellPackages =
        if compiler == "default"
        then nixpkgs.haskellPackages
        else nixpkgs.haskell.packages.${compiler};

    mkPackage = super: pkg: path: opts: inShell:
                let orig = super.callCabal2nixWithOptions pkg path opts {};
                 in if inShell
                    # Avoid copying the source directory to nix store by using
                    # src = null.
                    then orig.overrideAttrs (oldAttrs: { src = null; })
                    else orig;

    mkHaskellPackages = inShell:
        haskellPackages.override {
            overrides = self: super:
                with nixpkgs.haskell.lib;
                {
                    bench-show = super.callHackageDirect {
                      pkg = "bench-show";
                      ver = "0.3.2";
                      sha256 = "16b8vyzdp9b5bh34kqmbfwjsyv8wgnxxwl8kjcpgxjsh52xzyaa0";
                    } { };

                    bench-report = mkPackage super "bench-report" ./. "" inShell;
                };
        };

    drv = mkHaskellPackages true;

    shell = drv.shellFor {
        packages = p:
          [ p.bench-report
          ];
        # Use a better prompt
        shellHook = ''
          export CABAL_DIR="$(pwd)/.cabal.nix"
          if test -n "$PS_SHELL"
          then
            export PS1="$PS_SHELL\[$bldred\](nix)\[$txtrst\] "
          fi
        '';
    };
in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).bench-report
