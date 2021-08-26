-module(ecolor).

-export([]).

-include("ecolor.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% TYPES.
%%

-type color() :: black
               | blue
               | cyan
               | green
               | magenta
               | red
               | white
               | yellow
               | default
               | unset.

-type text_style() :: bold
                    | dim
                    | italic
                    | underline
                    | blinking.

-record(style, {text_style = []    :: [text_style()],
                foreground = unset :: color(),
                background = unset :: color()
               }).

-type style() :: #style{}.
-type sgr_attribute() :: binary().

%%
%% PUBLIC API.
%%

%% Applies the given foreground color to the given string or style.
-spec set_foreground(color(), string() | style()) -> string() | style().
set_foreground(Color, Style) when is_record(Style, style) ->
    Style#style{foreground = Color};
set_foreground(Color, String) ->
    Style = #style{foreground = Color},
    set_style(Style, String).

%% Applies the given background color to the given string or style.
-spec set_background(color(), string() | style()) -> string() | style().
set_background(Color, Style) when is_record(Style, style) ->
    Style#style{background = Color};
set_background(Color, String) ->
    Style = #style{background = Color},
    set_style(Style, String).

%% Applies the given text style (or a list of text styles) to the given
%% string or style.
-spec set_text_style(text_style() | [text_style()],
                     string() | style()
                    ) -> string() | style().
set_text_style(S, Style) when is_atom(S), is_record(Style, style) ->
    Style#style{text_style = [S]};
set_text_style(S, Style) when is_list(S), is_record(Style, style) ->
    Style#style{text_style = S};
set_text_style(S, String) when is_atom(S) ->
    Style = #style{text_style = [S]},
    set_style(Style, String);
set_text_style(S, String) when is_list(S) ->
    Style = #style{text_style = S},
    set_style(Style, String).

%% Applies the given style to the given string.
-spec set_style(style(), string()) -> string().
set_style(Style, String) when is_record(Style, style) ->
    case sgr(Style) of
        <<>> ->
            String;
        Seq ->
            [Seq, String, reset()]
    end.

%%
%% HELPERS.
%%

%% Constructs a SGR sequence from the given style, the given list of
%% attributes.
-spec sgr(binary() | [binary()] | style()) -> binary().
sgr(<<>>) ->
    <<>>;
sgr(Attributes) when is_binary(Attributes) ->
    <<?CSI/binary, Attributes/binary, "m">>;
sgr(Attributes) when is_list(Attributes) ->
    Bin = join_attributes(Attributes),
    sgr(Bin);
sgr(#style{text_style = TS, foreground = FG, background = BG}) ->
    T = text_style(TS),
    F = foreground(FG),
    B = background(BG),
    sgr([T, F, B]).

%% Returns the SGR reset sequence to reset all attributes to their
%% defaults.
-spec reset() -> binary().
reset() ->
    sgr(?RESET_CODE).

%% Returns text style sequence from the given list of styles.
-spec text_style([text_style()]) -> binary().
text_style(_Styles) ->
    <<>>.

%% Returns foreground color sequence based on the given color code.
-spec foreground(color()) -> binary().
foreground(black) ->
    ?FOREGROUND_BLACK;
foreground(blue) ->
    ?FOREGROUND_BLUE;
foreground(cyan) ->
    ?FOREGROUND_CYAN;
foreground(green) ->
    ?FOREGROUND_GREEN;
foreground(magenta) ->
    ?FOREGROUND_MAGENTA;
foreground(red) ->
    ?FOREGROUND_RED;
foreground(white) ->
    ?FOREGROUND_WHITE;
foreground(yellow) ->
    ?FOREGROUND_YELLOW;
foreground(default) ->
    ?FOREGROUND_DEFAULT;
foreground(_) ->
    <<>>.

%% Returns background color sequence based on the given color code.
-spec background(color()) -> binary().
background(black) ->
    ?BACKGROUND_BLACK;
background(blue) ->
    ?BACKGROUND_BLUE;
background(cyan) ->
    ?BACKGROUND_CYAN;
background(green) ->
    ?BACKGROUND_GREEN;
background(magenta) ->
    ?BACKGROUND_MAGENTA;
background(red) ->
    ?BACKGROUND_RED;
background(white) ->
    ?BACKGROUND_WHITE;
background(yellow) ->
    ?BACKGROUND_YELLOW;
background(default) ->
    ?BACKGROUND_DEFAULT;
background(_) ->
    <<>>.

%% Joins several Select Graphic Rendition (SGR) attributes together
%% (with semicolon).
-spec join_attributes([sgr_attribute()]) -> sgr_attribute().
join_attributes(Attributes) ->
    R = fun(Elem, <<>>) ->
                Elem;
           (<<>>, Acc)  ->
                Acc;
           (Elem, Acc) ->
                <<Acc/binary, ?ATTRIBUTE_SEPARATOR/binary, Elem/binary>>
        end,
    lists:foldl(R, <<>>, Attributes).

%%
%% TESTS.
%%

-ifdef(TEST).

-define(
   TFB(X, Y, Z),
   #style{text_style = X, foreground = Y, background = Z}
  ).

-define(TS(X), #style{text_style = X}).
-define(FG(X), #style{foreground = X}).
-define(BG(X), #style{background = X}).

contruct_sgr_seq_from_style_test_() ->
    Tests = [
             %% foreground color
             {?FG(black),     <<"\e[30m">>},
             {?FG(red),       <<"\e[31m">>},
             {?FG(green),     <<"\e[32m">>},
             {?FG(yellow),    <<"\e[33m">>},
             {?FG(blue),      <<"\e[34m">>},
             {?FG(magenta),   <<"\e[35m">>},
             {?FG(cyan),      <<"\e[36m">>},
             {?FG(white),     <<"\e[37m">>},
             {?FG(default),   <<"\e[39m">>},
             {?FG(unset),     <<>>},

             %% background color
             {?BG(black),     <<"\e[40m">>},
             {?BG(red),       <<"\e[41m">>},
             {?BG(green),     <<"\e[42m">>},
             {?BG(yellow),    <<"\e[43m">>},
             {?BG(blue),      <<"\e[44m">>},
             {?BG(magenta),   <<"\e[45m">>},
             {?BG(cyan),      <<"\e[46m">>},
             {?BG(white),     <<"\e[47m">>},
             {?BG(default),   <<"\e[49m">>},
             {?BG(unset),     <<>>}
            ],
    [?_assertEqual(E, sgr(I)) || {I, E} <- Tests].

join_attributes_test_() ->
    Tests = [
             {[], <<>>},
             {[<<"a">>], <<"a">>},
             {[<<"a">>, <<"b">>], <<"a;b">>},
             {[<<"a">>, <<"b">>, <<"c">>], <<"a;b;c">>},
             {[<<"1">>, <<"31">>], <<"1;31">>},
             {[<<"1">>, <<"37">>, <<"41">>], <<"1;37;41">>},
             {[<<>>, <<"37">>, <<"41">>], <<"37;41">>},
             {[<<>>, <<"37">>, <<>>], <<"37">>}
            ],
    [?_assertEqual(E, join_attributes(I)) || {I, E} <- Tests].

-endif.
