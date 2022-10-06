{
  description = "Mpv Client";

  inputs = {
    hix.url = git+https://git.tryp.io/tek/hix;
    prelate.url = git+https://git.tryp.io/tek/prelate;
  };

  outputs = { hix, prelate, ... }:
  let
    overrides = { hackage, source, jailbreak, unbreak, ... }:
    {
    };

  in hix.lib.pro ({ config, lib, ... }: {
    base = ./.;
    inherit overrides;
    depsFull = [prelate];
    packages.mpv = ./packages/mpv;
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    devGhc.compiler = "ghc902";
    ghci = {
      args = ["-fplugin=Polysemy.Plugin"];
      preludePackage = "prelate";
      preludeModule = "Prelate";
    };
  });
}
