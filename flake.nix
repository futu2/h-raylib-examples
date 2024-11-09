{
  description = "h-raylib examples";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    hix.url = "github:tek/hix?ref=0.7.1";
    hix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {hix, ...}: hix.lib.flake {
    hackage.versionFile = "ops/version.nix";

    compiler = "ghc98";

    overrides = { hackage, cabalOverrides, ... }: {
      "h-raylib" = cabalOverrides {c = null;} (hackage "5.5.2.1" "sha256-kn8S/jmfn4pAw2bZ6LwkzqIu5W24gOpnvFPUpUKHJFQ=");
    };

    cabal = {
      license = "MIT";
      license-file = "LICENSE";
      author = "h-raylib";
      ghc-options = ["-Wall"];
      language="GHC2021";
    };

    packages.h-raylib-examples = {
      src = ./.;
      cabal.meta.synopsis = "h-raylib examples";

      library = {
        enable = true;
        source-dirs = "src";
        dependencies = [
          "h-raylib >= 5 && < 6"
          "lens >= 5 && < 6"
          "linear >= 1 && < 2"
          "random >= 1 && < 2"
        ];
      };
      executable = {
        enable = true;
        source-dirs = "app";
      };

    };
  };
}
