{
  description = "Mpv Client";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
  };

  outputs = { hix, ... }: hix.lib.pro ({config, ...}: {
    hackage.versionFile = "ops/version.nix";

    overrides = {hackage, jailbreak, ...}: {
      exon = hackage "1.6.1.0" "0kl18fl0vd442rbk6ipzln0z654ybd8k50v58y3a7zdp2c4yj0sv";
      incipit = hackage "0.9.0.1" "13qp45wry6xs54fhkcvydnz9b3nqd88sg1ypg5kpl9af4z9gqd3s";
      polysemy = hackage "1.9.1.2" "01vkiqxcjvvihgg8dvws76sfg0d98z8xyvpnj3g3nz02i078xf8j";
      polysemy-chronos = hackage "0.6.0.2" "1wvjpl2axxhywjj7z1hjg16sxldq0x63md4rzf1mvdn8067mg35s";
      polysemy-conc = hackage "0.13.0.1" "01zfjx1kmrw5hnqyckrwwkdzjbihfn6y516lw7lffhqfp354522b";
      polysemy-http = jailbreak (hackage "0.13.0.1" "0zg9dhkbsy3sn7gs0axrn4y9z12jqn1138lbcz4lis0s8fjh0zj2");
      polysemy-log = hackage "0.10.0.1" "1vwlj7xpr4v4340mx8ylfrn2wikix0lkbhg86bikpkzhhk1w3q7q";
      polysemy-plugin = hackage "0.4.5.1" "0afmx1vdgmvggk4sb4av91qnm8b3hr2kb4adcj9fhzq2w50393bc";
      polysemy-process = hackage "0.13.0.1" "0jzcr0vvmnmpvyyk062lq1k4xcyph9zn6b80wwn6h484qjpwpqcd";
      polysemy-resume = hackage "0.8.0.1" "1fci0v1xc6xx8qkj8s57m7yy2w1rxyxvb9bw9vkksdxr3z38dbkg";
      polysemy-test = hackage "0.9.0.0" "1adkp48v04klsjyv8846w7ryf1fiqxb4ga69mps9vg2bp9fj5i7j";
      polysemy-time = hackage "0.6.0.2" "198x2wimdzk93hz0bq2k7wjibcjvzm38m6fica1jfcbh4p531swp";
      prelate = jailbreak (hackage "0.7.0.1" "0qy0dkckvlbinp1gm85ziiyml0lj57b93qnz23ldjmbj4skcp8s8");
      zeugma = hackage "0.9.0.1" "1clsd2c26cp60kajf4aw8wydnmvgr4blka8yzysi3gzd8ky32ck1";
    };

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = "^>= 0.7";
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
          "exon >= 1.4 && <1.7"
          "network ^>= 3.1"
          "path ^>= 0.9"
          "path-io ^>= 1.7"
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
