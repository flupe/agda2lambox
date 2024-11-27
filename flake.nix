{
  description = "agda2lambox";

  inputs.nixpkgs.url = github:NixOS/nixpkgs;
  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = {self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {inherit system;};
        agda2lambox = pkgs.haskellPackages.callCabal2nix "agda2lambox" ./. {};
      in {
        packages = {
          inherit agda2lambox;
          default = agda2lambox;
        };
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [agda2lambox];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            cabal2nix
            haskell-language-server
            pkgs.agda
          ];
        };
      });
}
