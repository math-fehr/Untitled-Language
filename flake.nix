
{
  description = "Builds the Untitled Language™ compiler";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-20.03;
  };

  outputs = { self, nixpkgs }:
    let
      system           = "x86_64-linux";
      pkgs             = nixpkgs.legacyPackages.${system};
      compiler-set     = pkgs.haskell.packages.ghc8101;
      git-ignore       = pkgs.nix-gitignore.gitignoreSourcePure;
      package          = compiler-set.callCabal2nix "Untitled-Language" (git-ignore [ ./.gitignore ] ./.) {};
    in {
      packages.${system}.untitled-language = package;
      # {
      #   devShell = compiler-set.shellFor {
      #     packages = p: [ package ];
      #     buildInputs = with pkgs; [
      #       compiler-set.cabal-install
      #     ];
      #   };
      # };
      defaultPackage.${system} = self.packages.${system}.untitled-language;
    };
}

