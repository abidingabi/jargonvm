{
  description = "A toy JVM.";

  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };

  outputs = { self, nixpkgs }:
    let
      # User-friendly version number
      version = builtins.substring 0 8 self.lastModifiedDate;

      # System types to support
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in {
      packages = forAllSystems (system: rec {
        jargonvm = nixpkgsFor.${system}.rustPlatform.buildRustPackage {
          pname = "jargonvm";
          inherit version;
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
        };

        default = jargonvm;
      });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.jargonvm}/bin/jargonvm";
        };
      });

      devShells = forAllSystems (system: {
        default = let pkgs = nixpkgsFor.${system};
        in pkgs.mkShell {
          buildInputs = with pkgs; [ cargo rustc rustfmt rustPackages.clippy ];
        };
      });
    };
}
