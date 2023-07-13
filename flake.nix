{
  description = "Mpv Client";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    prelate.url = "git+https://git.tryp.io/tek/prelate";
  };

  outputs = { hix, prelate, ... }: hix.lib.pro ({config, ...}: {
    hackage.versionFile = "ops/version.nix";
    depsFull = [prelate];

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = "^>= 0.6";
        };
        module = "Prelate";
      };
      paths = false;
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Video";
        github = "tek/mpv";
        extra-source-files = ["changelog.md" "readme.md"];
      };
      dependencies = ["polysemy ^>= 1.9" "polysemy-plugin ^>= 0.4.5"];
      ghc-options = ["-fplugin=Polysemy.Plugin"];
    };

    packages.mpv = {
      src = ./packages/mpv;
      cabal.meta.synopsis = "Mpv Client for Polysemy";
      library = {
        enable = true;
        dependencies = [
          "aeson >= 2.0 && < 2.2"
          "dependent-sum-template ^>= 0.1"
          "exon ^>= 1.4"
          "network ^>= 3.1"
          "path ^>= 0.9"
          "path-io ^>= 1.7"
          "polysemy-conc ^>= 0.12"
          "some ^>= 1.0"
          "sop-core ^>= 0.5"
          "template-haskell"
          "typed-process ^>= 0.2"
        ];
      };
      test = {
        enable = true;
        dependencies = [
          "hedgehog >= 1.1 && < 1.3"
          "path ^>= 0.9"
          "polysemy-test ^>= 0.7"
          "tasty ^>= 1.4"
          "tasty-expected-failure ^>= 0.12"
          "zeugma ^>= 0.8"
        ];
      };
    };

  });
}
