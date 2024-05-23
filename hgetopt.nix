{
  pkgs ? import <nixpkgs> { },
}:
let
  inherit (pkgs) sbcl callPackage;
  inherit
    (sbcl.withPackages (
      ps: with ps; [
        alexandria
        toadstool
      ]
    ))
    lispLibs
    ;
in
sbcl.buildASDFSystem {
  pname = "hgetopt";
  version = "0.0.4";
  src = pkgs.fetchzip {
    url = "http://pauldist.kisp.in/archive/hgetopt-0.0.4.tgz";
    sha256 = "FJ9bgH8SmyVNUFsGBc3jNX++FIuoF5gumr5vXeCIqJw=";
  };

  systems = [ "hgetopt" ];

  lispLibs = lispLibs;
}
