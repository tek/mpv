{
  description = "Mpv Client";

  inputs.hix.url = github:tek/hix;
  inputs.polysemy-conc.url = github:tek/polysemy-conc;

  outputs = { hix, polysemy-conc, ...  }:
  let
    overrides = { hackage, source, jailbreak, ... }:
    {
      co-log-polysemy = jailbreak (hackage "0.0.1.2" "17bcs8dvrhwfcyklknkqg11gxgxm2jaa7kbm6xx4vm1976abzwss");
      exon = hackage "0.1.0.0" "0lwq53zcw6v030yk0v7p6s5cv1gqag2jb56lh3p7xc5qdn06bc6b";
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-conc = source.package polysemy-conc "conc";
      polysemy-plugin = hackage "0.4.0.0" "0pah1a8h8ckbv2fq20hrikrd1p5a3bdxr03npkyixc6mv5k1rmck";
      polysemy-test = hackage "0.3.1.7" "0j33f5zh6gyhl86w8kqh6nm02915b4n32xikxc4hwcy7p5l7cl34";
      polysemy-time = hackage "0.1.3.2" "1s06c1jwsq9ckfcq2cwwpiy5a2a0lj8j63zg4jr2kidpd2lkk6cd";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
    };

  in hix.flake {
    base = ./.;
    inherit overrides;
    packages = {
      mpv = ./packages/mpv;
    };
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin"];
    ghcid.easy-hls = false;
    compat = false;
  };
}
