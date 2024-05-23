{
  pkgs ? import <nixpkgs> { },
}:
let
  inherit (pkgs) sbcl callPackage;
  trivial-template = callPackage ./trivial-template.nix { };
  hgetopt = callPackage ./hgetopt.nix { };
  inherit
    (sbcl.withPackages (
      ps: with ps; [
        cl-fad
        cl-ppcre
        trivial-template
        hgetopt
      ]
    ))
    lispLibs
    ;
in
sbcl.buildASDFSystem {
  pname = "document-templates";
  version = "0.0.17";
  src =
    let
      patterns = ''
        *
        !document-templates.asd
        !version.lisp-expr
        !package.lisp
        !document-templates.lisp
      '';
    in
    pkgs.nix-gitignore.gitignoreSourcePure patterns ./.;

  systems = [ "document-templates" ];

  lispLibs = lispLibs;
}
