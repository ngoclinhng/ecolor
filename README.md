ecolor
=====

[ANSI](https://en.wikipedia.org/wiki/ANSI_escape_code#SGR) display attributes
for your Erlang.

Install
-------

Add the following line to the `deps` list in your project `rebar.config` file.
    
    {ecolor, {git,"https://github.com/ngoclinhng/ecolor", {branch, "main"}}}

Usage
-----

**ecolor** has only a handful of functions that help you decorate your
terminal output, namely:

- `set_foreground/2`: sets foregound color. The first argument is the color
   you want to set, and the second argument is either your text data
   (`string()`, `iodata()`, etc...) or another style. [The eight standard
   ANSI colors](https://en.wikipedia.org/wiki/ANSI_escape_code#Colors) are
   supplied via the corresponding atoms (`black`, `red`, `green`, `yellow`,
   `blue`, `magenta`, `cyan`, `white`); [8-bit colors](https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit) are supplied via their corresponding bytes (any
   value between `0` and `255`); [24-bit colors](https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit) can be supplied via either a 3-element list
   `[R, G, B]` or a hex string (e.g., `#f5f6f7`, `f5f6f7`, `F5F6F7`).   

   ```erlang
   io:format("This is ~s color~n", [ecolor:set_foreground(green, "green")]).
   io:format("This is ~s 8-bit color~n", [ecolor:set_foreground(120, "greenish")]).

   %% The following three examples may not be supported by some terminal.
   io:format("This is ~s 24-bit color~n", [ecolor:set_foreground("#002B36", "#002B36")]).
   io:format("This is ~s 24-bit color~n", [ecolor:set_foreground("002B36", "#002B36")]).
   io:format("This is ~s 24-bit color~n", [ecolor:set_foreground([0, 43, 54], "rgb(0, 43, 54)")]).
   ```

   ***Note**: 24-bit colors (a.k.a RGB or true colors) are not widely
   supported.*

- `set_background/2`: sets background color. Arguments are exactly the same
  as those of `set_foreground/2`.

- `set_text_style/2`: sets text style. The first argument is the text style
  you want to set, and the second is either your text data or another style.
  Text style can be either one of the atoms `bold`, `dim`, `italic`,
  `underline`, and `blinking`, or a list of those atoms (e.g `[italic,
  underline]`).

  ```erlang
  io:format("This is ~s format~n", [ecolor:set_text_style(bold, "bold")]).

  %% This example may not work on some terminals.
  io:format("This is ~s format~n", [ecolor:set_text_style([italic, underline], "italic and underline")]).
  ```

  ***Note**: except for the `bold` format, other formats may not be supported
  by [some terminals](https://en.wikipedia.org/wiki/ANSI_escape_code#SGR).*

- `new_style/0` and `set_style/2`: these two functions allow you add more
   advanced styling to your text.

   ```erlang
   %% bold, white text on red background.
   S0    = ecolor:new_style(),
   S1    = ecolor:set_text_style(bold, S0),
   S2    = ecolor:set_foreground(white, S1),
   Style = ecolor:set_background(red, S2).

   io:format("This is ~s style~n", [ecolor:set_style(Style, "bold, white text on red background")]).
   ```