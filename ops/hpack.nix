{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = "0.1.0.0";
    description = "Please see the README on GitHub at <https://github.com/tek/mpv>";
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "haskell@tryp.io";
    copyright = "2023 Torsten Schmits";
    category = "Video";
    build-type = "Simple";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
    "-fplugin=Polysemy.Plugin"
  ];

  dependencies = [
    { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
    { name = "prelate"; version = "^>= 0.5.1"; mixin = ["(Prelate as Prelude)" "hiding (Prelate)"]; }
    "polysemy >= 1.6"
    "polysemy-plugin >= 0.4"
  ];

  project = name: merge (meta // { library = paths name; } // options) {
    inherit name;
    library = {
      source-dirs = "lib";
      inherit dependencies;
    };
    default-extensions = config.ghci.extensions;
  };

  exe = name: dir: merge (paths name // {
    main = "Main.hs";
    source-dirs = dir;
    inherit dependencies;
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  mpv = merge (project "mpv") {
    synopsis = "Mpv Client for Polysemy";
    library.dependencies = [
      "aeson >= 1.4"
      "dependent-sum-template"
      "exon"
      "lens"
      "network"
      "path"
      "path-io"
      "polysemy >= 1.5"
      "polysemy-conc"
      "polysemy-log >= 0.3.0.2"
      "polysemy-plugin >= 0.3"
      "polysemy-time"
      "some"
      "sop-core"
      "template-haskell"
      "typed-process"
    ];
    tests = {
      mpv-unit = exe "mpv" "test" {
        dependencies = [
          "hedgehog"
          "mpv"
          "path"
          "polysemy >= 1.5"
          "polysemy-conc"
          "polysemy-log"
          "polysemy-plugin >= 0.3"
          "polysemy-test"
          "polysemy-time"
          "tasty"
          "tasty-expected-failure"
        ];
      };
    };
  };

}
