{
  description = "Mpv Client";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    hls.url = "github:haskell/haskell-language-server?ref=1.9.0.0";
    prelate.url = "git+https://git.tryp.io/tek/prelate";
  };

  outputs = { hix, prelate, ... }:
  hix.lib.pro ({ config, lib, ... }: {
    depsFull = [prelate];
    packages.mpv = ./packages/mpv;
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    devGhc.compiler = "ghc925";
    ghci = {
      args = ["-fplugin=Polysemy.Plugin"];
      preludePackage = "prelate";
      preludeModule = "Prelate";
    };
  });
}
