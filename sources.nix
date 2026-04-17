{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  streamly = local ./.;
  streamly-core = local ./core;
  streamly-benchmarks = local ./benchmark
    // { c2nix = ["--flag fusion-plugin --flag inspection"];
         flags = ["--flags fusion-plugin"];
       };
  streamly-tests = local ./test
    // { c2nix = ["--flag fusion-plugin"];
         flags = ["--flags fusion-plugin"];
       };
  bench-test-lib = local ./bench-test-lib;

  network = hackage "3.2.7.0" "08frm9gm422b9aqlmmzflj0yr80ic0ip8s4gsmr0izhizzab5420";
}
];
}
