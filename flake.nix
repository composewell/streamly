{
  description = "Streamly";

  inputs = {
    basepkgs.url = "git+ssh://git@github.com/composewell/streamly-packages?rev=69728978adc44f53b3dd907acb2eb5bd2415fd60";
    nixpkgs.follows = "basepkgs/nixpkgs";
    nixpkgs-darwin.follows = "basepkgs/nixpkgs-darwin";
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    let
      outputs = basepkgs.nixpack.mkOutputs {
        inherit nixpkgs nixpkgs-darwin basepkgs;
        name = "streamly";
        sources = import ./sources.nix;
        packages = import ./packages.nix;
      };
    in outputs // {
      # GHC JavaScript backend shell: nix develop .#js
      # It uses nixpkgs without the overlays of the default shell, so that
      # its packages come from the binary cache where available.
      devShells = builtins.mapAttrs (system: shells:
        shells // {
          js = import ./packages-js.nix
            { nixpkgs = nixpkgs.legacyPackages.${system}; };
        }) outputs.devShells;
    };
}
