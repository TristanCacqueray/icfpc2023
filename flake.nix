{
  nixConfig.bash-prompt = "[nix(jkrt)] ";
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/fe0dabfd8acf96f1b5cff55766de6284517868cf";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;

      haskellExtend = hpFinal: hpPrev: {
        progcon = hpPrev.callCabal2nix "progcon" "${self}/progcon-hs" { };
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

      ciTools = with pkgs; [
        cabal-install
        hlint
        fourmolu
        weeder
        hsPkgs.doctest
      ];
      devTools = with pkgs; [
ghcid haskell-language-server feh
      ];

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
