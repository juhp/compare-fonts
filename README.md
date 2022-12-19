# compare-fonts

## Usage

```shellsession
$ compare-fonts --lang ja
```

![screenshot](compare-fonts.png)

Uses Haskell
[gi-gtk-declarative](https://hackage.haskell.org/package/gi-gtk-declarative).

## Usage
```shellsession
$ compare-fonts --help
compare-fonts

Usage: compare-fonts [--version] [(-t|--text TEXT) | (-l|--lang LANG)]
                     [-W|--width WIDTH] [-H|--height HEIGHT]
                     [-m|--margin MARGIN]
                     [(-1|--font1 WORDS) | --font-family1 FONT]
                     [(-2|--font2 WORDS) | --font-family2 FONT] [-f|--use-face]
                     [-s|--font-size SIZE] [-w|--wrap] [--hide-font-size]
  GUI tool to compare two fonts

Available options:
  -h,--help                Show this help text
  --version                Show version
  -t,--text TEXT           text to display
  -l,--lang LANG           sample text by language
  -W,--width WIDTH         Window width
  -H,--height HEIGHT       Window height
  -m,--margin MARGIN       Margin size [default 10]
  -1,--font1 WORDS         Match 1st font words
  --font-family1 FONT      1st font [default Sans] [default: "Sans"]
  -2,--font2 WORDS         Match 2nd font words
  --font-family2 FONT      2nd font [default Serif] [default: "Serif"]
  -f,--use-face            Use face results rather than families
  -s,--font-size SIZE      Font size [default 16]
  -w,--wrap                Enable text wrapping
  --hide-font-size         Hide font size in FontButtons
```

## Installation

On Fedora install `ghc-gi-gtk-devel` and then run `cabal install`.

Can also build with `stack install` if your Harfbuzz not too new.
