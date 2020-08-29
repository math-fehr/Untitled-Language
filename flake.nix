
{
  description = "Builds the Untitled Language™ compiler";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  };

  outputs = { self, nixpkgs }:
    let
      system           = "x86_64-linux";
      pkgs             = nixpkgs.legacyPackages.${system};
      git-ignore       = pkgs.nix-gitignore.gitignoreSourcePure;
      compiler-set     = pkgs.haskell.packages.ghc8101.override {
        overrides = self: super: {
          untitled-language = super.callCabal2nix "Untitled-Language" (git-ignore [ ./.gitignore ] ./.) {};
        };
      };
      shell            = compiler-set.shellFor {
        packages = hpkgs: [ hpkgs.untitled-language ];
        withHoogle = false;
        buildInputs = with compiler-set; [
          cabal-install hlint
        ];
      };
    in {
      packages.${system} = {
        inherit (compiler-set) untitled-language;
      };
      defaultPackage.${system} = self.packages.${system}.untitled-language;
      devShell.${system} = shell;
    };
}

