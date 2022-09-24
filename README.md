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
# motivations

* avoid using LaTeX

# usage

``` sh
nix build github:djanatyn/resume
```

# credits

Icons used are from [Simple Icons](https://github.com/simple-icons/simple-icons) (CC0):
* [Haskell Logo](https://github.com/simple-icons/simple-icons/blob/521c96fd04b0ea93034db8715eda5a4de27a58bb/icons/haskell.svg)
* [NixOS Logo](https://github.com/simple-icons/simple-icons/blob/521c96fd04b0ea93034db8715eda5a4de27a58bb/icons/nixos.svg)
* [Elm Logo](https://github.com/simple-icons/simple-icons/blob/521c96fd04b0ea93034db8715eda5a4de27a58bb/icons/elm.svg)
* [Rust Logo](https://github.com/simple-icons/simple-icons/blob/521c96fd04b0ea93034db8715eda5a4de27a58bb/icons/rust.svg)
