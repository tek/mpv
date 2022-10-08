{
  description = "Mpv Client";

  inputs = {
    hix.url = git+https://git.tryp.io/tek/hix;
    prelate.url = git+https://git.tryp.io/tek/prelate;
    polysemy-conc.url = git+https://git.tryp.io/tek/polysemy-conc;
  };

  outputs = { hix, prelate, polysemy-conc, ... }:
  let
    overrides = { hackage, source, jailbreak, unbreak, ... }:
    {
      polysemy-process = source.package polysemy-conc "process";
    };

  in hix.lib.pro ({ config, lib, ... }: {
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
