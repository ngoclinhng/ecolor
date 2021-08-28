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
   you want to set, and the second argument is your text data (`string`,
   `iodate`, etc...).

- `set_background/2`: sets background color.

- `set_text_style/2`: sets text style.