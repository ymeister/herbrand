{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix
}:

let project = import ./default.nix { inherit system inputs; };

in project.shell
