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
        # Bump requested at https://github.com/ekmett/gl/issues/24
        gl = pkgs.haskell.lib.doJailbreak hpPrev.gl;
        massiv = pkgs.haskell.lib.doJailbreak
          (pkgs.haskell.lib.overrideCabal hpPrev.massiv {
            patches = [
              (pkgs.fetchpatch {
                name = "p1";
                url =
                  "https://github.com/lehins/massiv/commit/040afd7d57f90b5fe1090654005f9ca506ce3bd9.patch";
                sha256 = "sha256-j/BvGL/Bd+dknbZ4FThPLOWvelUUtrQiQ6tcRHzEnUY=";
                stripLen = 1;
              })
              (pkgs.fetchpatch {
                name = "p2";
                url =
                  "https://github.com/lehins/massiv/commit/9eb03ebe03f658a3401ec2a26a55be175eeb390c.patch";
                sha256 = "sha256-tj5taGaoKzNrKtwaULur+SAlRT/powjp4TSdxwZMFF4=";
                stripLen = 1;
              })
            ];
          });
        dear-imgui = pkgs.haskell.lib.doJailbreak hpPrev.dear-imgui;
        pvar = pkgs.haskell.lib.dontCheck hpPrev.pvar;
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
