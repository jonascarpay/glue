let
  pkgs = import <nixpkgs> {};
  hsPkgs = import ./default.nix {};
in
  hsPkgs.shellFor {
    packages = ps: [ ps.gb2 ];
    withHoogle = true;
    buildInputs = map (str: hsPkgs."${str}".components.exes."${str}") [
      "ghcid"
      # "stylish-haskell"
      # "hlint"
    ] ++ [
      pkgs.haskellPackages.stylish-haskell
    ];
    exactDeps = true;
  }


