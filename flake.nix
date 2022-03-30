{
  description = "Mpv Client";

  inputs.hix.url = github:tek/hix;
  inputs.polysemy-conc.url = github:tek/polysemy-conc;

  outputs = { hix, polysemy-conc, ... }:
  let
    overrides = { hackage, source, jailbreak, unbreak, ... }:
    {
      exon = hackage "0.3.0.0" "0jgpj8818nhwmb3271ixid38mx11illlslyi69s4m0ws138v6i18";
      flatparse = hackage "0.3.2.0" "01w71985b9ndg4wkfxqxjj7f1cynji6vp71akr7ivpmxn2drxspa";
      incipit = hackage "0.2.1.0" "1rxry273zv4h7rg29wwbj1caiaa56zi7f08bf0l9m5kj68y7c7i8";
      polysemy-conc = hackage "0.7.0.0" "1nin6k5vcpj9lll9ravk42rpbdymrjaawvzbdc8b2bivf39d2dfj";
      polysemy-log = hackage "0.6.0.1" "18p5sl304nf7pf6b8bfvbkwp1jsms24ym9hh3dmsppxk38fljixj";
    };

  in hix.lib.flake ({ config, lib, ... }: {
    base = ./.;
    inherit overrides;
    deps = [polysemy-conc];
    packages.mpv = ./packages/mpv;
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghci = {
      args = ["-fplugin=Polysemy.Plugin"];
      preludePackage = "incipit";
    };
  });
}
