{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  bench-runner = local ./.;
  streamly-targets = local ../../targets;
}
];
}
