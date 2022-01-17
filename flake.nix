{
  inputs = { nixpkgs.url = "nixpkgs"; };

  description = "resume for jonathan strickland";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in {

      packages = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          resume = pkgs.stdenv.mkDerivation {
            name = "resume";
            version = "0.1.0";
            src = ./src;

            buildInputs = with pkgs.haskell;
              [
                (packages.ghc902.ghcWithPackages (haskellPackages:
                  with haskellPackages; [
                    pandoc
                    pandoc-types
                    typed-process
                    bytestring
                    text
                  ]))
              ];

            buildPhase = ''
              export RESUME_NIXPKGS_REV="${nixpkgs.rev}"

              (cd $src && runhaskell Main.hs) >> resume.txt
            '';

            installPhase = ''
              mkdir -p $out && install -Dm755 resume.txt $out/resume.txt
            '';

          };
        });
    };
}
