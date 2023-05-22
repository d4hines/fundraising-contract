{
  inputs = {
    ligo-nix.url = "github:ulrikstrid/ligo-nix";
    nixpkgs.follows = "ligo-nix/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { self
    , ligo-nix
    , nixpkgs
    , flake-utils
    ,
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ ligo-nix.overlays.default ];
      };
    in
    {
      devShell = with pkgs;
        mkShell {
          packages = [ ligo ];
        };
    });
}
