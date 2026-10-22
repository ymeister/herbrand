{
  inputs = {
    self.submodules = true;

    nix-haskell.url = ./deps/nix-haskell;

    nixpkgs.follows = "nix-haskell/nixpkgs";

    flake-compat.follows = "nix-haskell/flake-compat";
  };

  outputs = inputs@{ self, ... }:
    let nixpkgs =
          if inputs ? "nixpkgs"
          then inputs.nixpkgs
          else builtins.getFlake "nixpkgs";

        eachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;

        projectFor = system: import ./default.nix { inherit system inputs; };

        shellFor = system: import ./shell.nix { inherit system inputs; };

        checksFor = system: import ./checks.nix { inherit system inputs; };
    in {
      packages = eachSystem (system:
        let project = projectFor system;
        in {
          default = project.hsPkgs.herbrand.components.library;
        });

      devShells = eachSystem (system: {
        default = shellFor system;
      });

      checks = eachSystem checksFor;
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nixcache.reflex-frp.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = "true";
  };
}
