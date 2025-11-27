{ nixpkgs }:
{
# General packages, haskell or non-haskell will be installed in the
# environment.
packages =
  with nixpkgs.pkgs;
    [
    ];

# Haskell packages available in the nix shell, the dependencies of these
# packages are also installed and exposed in ghc package database.
libraries =
with nixpkgs.haskellPackages;
[
];

# Haskell dev packages. Install only dependencies of these in the shell,
# do not build the packages themselves.
dev-packages =
  with nixpkgs.haskellPackages;
  [streamly streamly-core streamly-tests streamly-benchmarks bench-test-lib ];

}
