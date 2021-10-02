{
  description = "Mpv Client";

  inputs.hix.url = github:tek/hix;

  outputs = { hix, ... }:
  let
    overrides = { hackage, source, jailbreak, ... }:
    {
      co-log-polysemy = jailbreak (hackage "0.0.1.2" "17bcs8dvrhwfcyklknkqg11gxgxm2jaa7kbm6xx4vm1976abzwss");
      exon = hackage "0.1.0.0" "0lwq53zcw6v030yk0v7p6s5cv1gqag2jb56lh3p7xc5qdn06bc6b";
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-conc = hackage "0.4.0.1" "16i03j2s0lg306j2if62nhlsdvyyp40sz2khzsblz1a3flx5i7sf";
      polysemy-log = hackage "0.2.2.4" "1fgn7ywifbp02lz2wyaixvp43vnrff8n5nkczxmq1r5bzqbs6f45";
      polysemy-log-co = hackage "0.2.2.4" "006pw9zddacckr4f6l6dmr03glbj70zsmfydird1jz18xh37gvyf";
      polysemy-plugin = hackage "0.4.0.0" "0pah1a8h8ckbv2fq20hrikrd1p5a3bdxr03npkyixc6mv5k1rmck";
      polysemy-test = hackage "0.3.1.7" "0j33f5zh6gyhl86w8kqh6nm02915b4n32xikxc4hwcy7p5l7cl34";
      polysemy-time = hackage "0.1.4.0" "0hwx89cilmsdjs3gb5w6by87ysy24scgj5zg77vbfnqpzr3ifrwh";
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
