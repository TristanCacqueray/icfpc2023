{
  nixConfig.bash-prompt = "[nix(jkrt)] ";
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/3176a8460ff51d1fa223e2962b11bd85543a36ba";
  };
  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };

      haskellExtend = hpFinal: hpPrev: {
        progcon = hpPrev.callCabal2nix "progcon" self { };
        vector-shuffling = pkgs.haskell.lib.doJailbreak hpPrev.vector-shuffling;
      };
      hsPkgs = pkgs.haskellPackages.extend haskellExtend;

      ciTools = with pkgs; [
        cabal-install
        hlint
        pkgs.haskellPackages.fourmolu
      ];
      devTools = with pkgs; [ ghcid haskell-language-server ];

    in {
      haskellExtend = haskellExtend;
      packages."x86_64-linux".default =
        pkgs.haskell.lib.justStaticExecutables hsPkgs.progcon;
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.progcon ];
        buildInputs = ciTools ++ devTools;
      };
    };
}
