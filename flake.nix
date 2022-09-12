{
  inputs = { nixpkgs.url = "nixpkgs"; };

  description = "resume for jonathan strickland";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in {
      # dev shell just provides ghc 9.2
      devShells = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          resume = pkgs.mkShell {
            buildInputs = with pkgs; [
              haskell.compiler.ghc902
              zlib.dev
            ];
          };
        });

      # the package definition pulls in haskell deps through nixpkgs
      packages = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          resume = pkgs.stdenv.mkDerivation {
            name = "resume";
            version = "0.1.0";
            src = ./src;

            buildInputs = with pkgs.haskell; [
              (packages.ghc902.ghcWithPackages (haskellPackages:
                with haskellPackages; [
                  blaze-html
                  typed-process
                  bytestring
                  text
                  dhall
                ]))
              pkgs.wkhtmltopdf
            ];

            buildPhase = ''
              export RESUME_NIXPKGS_REV="${nixpkgs.rev}"
              runhaskell $src/Main.hs
            '';

            installPhase = ''
              mkdir -p $out && install -Dm755 resume.pdf $out/resume.pdf
            '';
          };
        });
    };
}
