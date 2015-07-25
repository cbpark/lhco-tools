with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, attoparsec, base, bytestring, conduit
             , conduit-extra, hep-utilities, pipes, pipes-attoparsec
             , pipes-bytestring, resourcet, stdenv, transformers
             }:
             mkDerivation {
               pname = "lhco-tools";
               version = "0.0.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 attoparsec base bytestring conduit conduit-extra hep-utilities
                 pipes pipes-attoparsec pipes-bytestring resourcet transformers
               ];
               homepage = "https://github.com/cbpark/lhco-tools";
               description = "Tools for the LHC Olympics (LHCO) data analyses using Haskell";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
