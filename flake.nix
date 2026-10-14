{
  inputs = {
    common.url = "github:nammayatri/common";
    nixpkgs.follows = "common/nixpkgs";
    flake-parts.follows = "common/flake-parts";
    haskell-flake.follows = "common/haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.common.flakeModules.ghc98
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          projectFlakeName = "tcp-streams";
          basePackages = config.haskellProjects.ghc98.outputs.finalPackages;
          autoWire = [ "packages" "checks" "devShells" "apps" ];
          settings = {
            tcp-streams = {
              check = false;
            };
          };
        };
        packages.default = self'.packages.tcp-streams;
      };
    };
}
