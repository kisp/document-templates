{
  pkgs ? import <nixpkgs> { },
}:
let
  inherit (pkgs) sbcl callPackage;
in
# inherit (sbcl.withPackages (ps: with ps; [ ])) lispLibs;
sbcl.buildASDFSystem rec {
  pname = "trivial-template";
  version = "0.0.1";
  src = pkgs.fetchzip {
    url = "http://pauldist.kisp.in/archive/trivial-template-${version}.tgz";
    sha256 = "zF+tzN7oj0CiCPO42wMoWm6ami+IADTuSvM1TO71fpQ=";
  };

  systems = [ "trivial-template" ];

  # lispLibs = lispLibs;
}
