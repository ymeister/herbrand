{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix
}:

let nix-haskell = import inputs.nix-haskell { inherit system inputs; };
    project = nix-haskell (import ./project.nix);

in project.haskell-nix.project
