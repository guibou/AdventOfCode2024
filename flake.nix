{
  description = "AoC 2024";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskellPackages;

        deps = [
          "ansi-terminal"
          "base"
          "base16-bytestring"
          "besout"
          "bytestring"
          "containers"
          "cryptohash"
          "directory"
          "Earley"
          "file-embed"
          "generic-deriving"
          "generic-lens"
          "hashable"
          "here"
          "hex"
          "sydtest"
          "sydtest-discover"
          "JuicyPixels"
          "lens"
          "linear"
          "matrix"
          "megaparsec"
          "memoize"
          "monad-memo"
          "monoidal-containers"
          "mtl"
          "optics-core"
          "parallel"
          "parser-combinators"
          "pqueue"
          "pretty-simple"
          "process"
          "PyF"
          "QuickCheck"
          "random"
          "relude"
          "split"
          "template-haskell"
          "text"
          "time"
          #"union-find"
          "unordered-containers"
          "vector"
          "weigh"
          "array"
          "Chart"
          "Chart-diagrams"
          "aeson-pretty"
          "range"
          "regex-tdfa"
          "lens-regex-pcre"
        ];

        extensions = [
          "BangPatterns"
          "BinaryLiterals"
          "DeriveAnyClass"
          "DeriveGeneric"
          "DerivingStrategies"
          "FlexibleContexts"
          "GeneralizedNewtypeDeriving"
          "LambdaCase"
          "MultiWayIf"
          "NamedFieldPuns"
          "OverloadedRecordDot"
          "OverloadedStrings"
          "PartialTypeSignatures"
          "PatternSynonyms"
          "QuasiQuotes"
          "ScopedTypeVariables"
          "StandaloneDeriving"
          "TemplateHaskell"
          "TupleSections"
          "TypeApplications"
          "ViewPatterns"
          "GHC2021"
          "NoFieldSelectors"
          "DuplicateRecordFields"
          "OverloadedRecordDot"
        ];

        flags = (builtins.map (e: "-X${e}") extensions)
          ++
          [
            "-isrc"
            "-Wall"

            # GHC is smart enough to infer correct type signatures most of the time
            # I don't really care about this warning
            "-Wno-missing-signatures"

            # I hate that one, everytime it forces me to rename my symbol
            # because of a stupid conflict and I break something
            "-Wno-name-shadowing"
            "-ilib"
            "-itests"
            "-itemplate"
          ];

        haskellPackages = pkgs.haskellPackages.extend (
          hfinal: hprev:
            with pkgs.haskell.lib;
            {

              besout = unmarkBroken (doJailbreak hprev.besout);
              #union-find = unmarkBroken (doJailbreak hprev.union-find);

            }
        );

        myGHC =
          (haskellPackages.ghcWithPackages (p:
            (builtins.map (i: p.${i}) deps)
          ));

        ghci = pkgs.writeShellScriptBin "ghci"
          ''
            ${myGHC}/bin/ghci ${toString flags} -interactive-print=Text.Pretty.Simple.pPrintLightBg
          '';

        mkBinary = name: mainFile: 
          pkgs.runCommand name
            {
              buildInputs = [ myGHC ];

            }
            ''
              set -x
              mkdir src lib content tests
              cp -r ${./src}/* src
              cp -r ${./lib}/* lib
              cp -r ${./content}/* content
              cp -r ${./tests}/* tests

              mkdir -p $out/bin
              ghc -O2 -threaded -rtsopts -with-rtsopts=-N ${toString flags} tests/Discover.hs ${mainFile} -o $out/bin/${name}
            '';
      in
      {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = [
              ghci
              myGHC
              hPkgs.haskell-language-server
            ];
          };
        };

        packages = {
          bench = mkBinary "bench" "tests/Bench.hs";

          haskell-hie-bios = pkgs.writeShellScriptBin "haskell-hie-bios"
            ''
              for i in ${toString flags}
              do
                echo $i >> $HIE_BIOS_OUTPUT
              done

              find -name "*.hs" >> $HIE_BIOS_OUTPUT
            '';
        };
      });
}
