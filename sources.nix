{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  streamly = local ./.;
  streamly-core = local ./core;
  streamly-benchmarks = localOpts ./benchmark
    ["--benchmark --flag fusion-plugin"]
    ["--flags fusion-plugin"];
  streamly-tests = localOpts ./test
    ["--flag fusion-plugin"]
    ["--flags fusion-plugin"];
  bench-test-lib = local ./bench-test-lib;
}
];
}
