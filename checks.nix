{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix
}:

let nix-haskell = import inputs.nix-haskell { inherit system inputs; };
    project = nix-haskell (import ./project.nix);
    pkgs = project.pkgs;
    ghc = project.haskell-nix.project.ghcWithPackages (ps: [ ps.herbrand ps.herbrand-logic ]);

in {
  readme = pkgs.runCommand "readme" { nativeBuildInputs = [ ghc ]; } ''
    ghc -Wall -outputdir . -o readme ${./README.lhs}
    ./readme > $out
  '';
}
