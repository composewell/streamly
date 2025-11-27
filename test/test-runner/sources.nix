{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  test-runner = local ./.;
  streamly-targets = local ../../targets;
}
];
}
