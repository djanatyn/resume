# resume

Build process for my work resume.

* Work experience recorded using [Dhall configuration language](https://dhall-lang.org/) (`src/experience.dhall`)
```dhall
let Resume
    : Type
    = { contact : ContactInfo
      , skills : Skills
      , history : List Job }
```
* `src/Main.hs` renders configuration into HTML document using [blaze-html](https://hackage.haskell.org/package/blaze-html) 
```haskell
renderJob :: Job -> BH.Html
renderJob Job {organization, position, duration, experiences} = BH.li $ do
  ...
```
* [wkhtmltopdf](https://wkhtmltopdf.org/) renders HTML into PDF
```
$ nix develop github:djanatyn/resume#resume -c wkhtmltopdf --version
wkhtmltopdf 0.12.6
```
* All dependencies + build automation managed using [Nix Flakes](https://nixos.wiki/wiki/Flakes)
```
buildPhase = ''
  export RESUME_NIXPKGS_REV="${nixpkgs.rev}"
  runhaskell $src/Main.hs
'';

installPhase = ''
  mkdir -p $out && install -Dm755 resume.pdf $out/resume.pdf
'';
```

# usage

``` sh
nix build github:djanatyn/resume
```
