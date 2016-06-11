let pkgs = import <nixpkgs> {};
    haskellPkgs = pkgs.haskellPackages;
in {
  myHaskellStackEnv = pkgs.myHaskellStackEnv;
}
