{
  description = "h-raylib examples";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    let
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; };

          haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: {
              h-raylib = self.callHackageDirect
                {
                  pkg = "h-raylib";
                  ver = "5.1.3.0";
                  sha256 = "sha256-KrhDxkyijeloIitYZ47v/7010uLKRKgKk1OBHlk7oDI=";
                }
                { c = null; };
              # pkgs.haskell.lib.doJailbreak
              # (pkgs.haskell.lib.unmarkBroken super.h-raylib);

            };
          };

          jailbreakUnbreak = pkg:
            pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

          packageName = "h-raylib-examples";
        in
        {
          packages.${packageName} = # (ref:haskell-package-def)
            haskellPackages.callCabal2nix packageName ./. rec {
              # Dependency overrides go here
            };

          defaultPackage = inputs.self.packages.${system}.${packageName};
          devShell = inputs.self.packages.${system}.${packageName}.env.overrideAttrs (oldEnv: { buildInputs = oldEnv.buildInputs ++ [ haskellPackages.haskell-language-server ]; });
        };
    in
    inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
