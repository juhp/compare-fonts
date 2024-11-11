# compare-fonts

A tool for comparing 2 fonts using Gtk3.

## Usage

```shellsession
$ compare-fonts --lang ja
```

![screenshot](compare-fonts.png)

Uses Haskell
[gi-gtk-declarative](https://hackage.haskell.org/package/gi-gtk-declarative).

## Usage
`$ compare-fonts --help`

```
compare-fonts

Usage: compare-fonts [--version] [(-t|--text TEXT) | (-l|--lang LANG)]
                     [-W|--width WIDTH] [-H|--height HEIGHT]
                     [-m|--margin MARGIN] [(-1|--font1 FAMILY) | --match1 WORDS]
                     [--style1 STYLE] [(-2|--font2 FAMILY) | --match2 WORDS]
                     [--style2 STYLE] [-S|--use-style] [-f|--use-face]
                     [-s|--font-size SIZE] [(-w|--wrap) | (-n|--no-wrap)]
                     [--hide-font-size] [--no-fallback]

  GUI tool to compare two fonts

Available options:
  -h,--help                Show this help text
  --version                Show version
  -t,--text TEXT           text to display
  -l,--lang LANG           sample text by language
  -W,--width WIDTH         Window width
  -H,--height HEIGHT       Window height
  -m,--margin MARGIN       Margin size [default 10]
  -1,--font1 FAMILY        1st font family
  --match1 WORDS           Match 1st font words
  --style1 STYLE           1st font style
  -2,--font2 FAMILY        2nd font family
  --match2 WORDS           Match 2nd font words
  --style2 STYLE           2nd font style
  -S,--use-style           List font styles
  -f,--use-face            Use face results rather than families
  -s,--font-size SIZE      Font size [default 24]
  -w,--wrap                Enable text wrapping
  -n,--no-wrap             Disable text wrapping
  --hide-font-size         Hide font size in FontButtons
  --no-fallback            Disable pango font fallback
```

## Installation

compare-fonts is packaged for Fedora in [copr](https://copr.fedorainfracloud.org/coprs/petersen/compare-fonts/).

On Fedora install `ghc-gi-gtk-devel` and then run `cabal install`.

Can also build with `stack install`.
