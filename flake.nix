{
  description = "Mpv Client";

  inputs.hix.url = github:tek/hix;
  inputs.polysemy-conc.url = github:tek/polysemy-conc;

  outputs = { hix, polysemy-conc, ... }:
  let
    overrides = { hackage, source, jailbreak, unbreak, ... }:
    {
      exon = hackage "0.2.0.1" "0hs0xrh1v64l1n4zqx3rqfjdh6czxm7av85kj1awya9zxcfcy5cl";
      flatparse = unbreak;
      polysemy-conc = hackage "0.5.0.0" "0dv3naixsv8fbwqdfa4j2wg1xjlrk1w80v6wclg96rlaisxgsxlg";
      polysemy-log = hackage "0.3.0.2" "075psivybvv2vjgna43nhc53w7ick68y1ycsc6qr45nwignjakfq";
    };

  in hix.flake {
    base = ./.;
    inherit overrides;
    deps = [polysemy-conc];
    packages.mpv = ./packages/mpv;
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin"];
    compat = false;
  };
}
