# resume

Build process for my work resume.

* Work experience recorded using [Dhall configurtion language](https://dhall-lang.org/) (`src/experience.dhall`)
```dhall
let Resume
    : Type
    = { contact : ContactInfo
      , skills : Skills
      , history : List Job }
```
* `src/Main.hs` renders configurtion into HTML document using [blaze-html](https://hackage.haskell.org/package/blaze-html) 
```haskell
renderJob :: Job -> BH.Html
renderJob Job {organization, position, duration, experiences} = BH.li $ do
  ...
```
* [wkhtmltopdf](https://wkhtmltopdf.org/) renders html into PDF
* All dependencies + build automation managed using [Nix Flakes](https://nixos.wiki/wiki/Flakes)

# usage

``` sh
nix build github:djanatyn/resume
```
