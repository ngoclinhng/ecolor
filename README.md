ecolor
=====

[ANSI](https://en.wikipedia.org/wiki/ANSI_escape_code#SGR) display attributes
for your Erlang.

Install
-------

    $ rebar3 compile

Usage
-----

**ecolor** has only a handful of functions that help you decorate your
terminal output, namely:

- `set_foreground/2`: sets foregound color. The first argument is the color
   you want to set, and the second argument is either your text data
   (`string()`, `iodata()`, etc...) or another style. [The eight standard
   ANSI colors](https://en.wikipedia.org/wiki/ANSI_escape_code#Colors) are
   supplied via the corresponding atoms (`black`, `red`, `green`, `yellow`,
   `blue`, `magenta`, `cyan`, `white`).

- `set_background/2`: sets background color. Arguments are exactly the same
  as those of `set_foreground/2`.

- `set_text_style/2`: sets text style.