-module(ecolor).

-export([set_foreground/2, set_background/2, set_text_style/2]).
-export([new_style/0, set_style/2]).

-include("ecolor.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% TYPES.
%%

-type rgb() :: [byte()].
-type hex() :: string().

-type color() :: black
               | blue
               | cyan
               | green
               | magenta
               | red
               | white
               | yellow
               | default
               | byte()
               | rgb()
               | hex()
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
-type data() :: unicode:chardata() | iodata().
-type data_or_style() :: data() | style().
-type text_styles() :: text_style() | [text_style()].

%%
%% PUBLIC API.
%%

%% Applies the given foreground color to the given data or style.
-spec set_foreground(color(), data_or_style()) -> data_or_style().
set_foreground(Color, Style) when is_record(Style, style) ->
    Style#style{foreground = Color};
set_foreground(Color, String) ->
    Style = #style{foreground = Color},
    set_style(Style, String).

%% Applies the given background color to the given data or style.
-spec set_background(color(), data_or_style()) -> data_or_style().
set_background(Color, Style) when is_record(Style, style) ->
    Style#style{background = Color};
set_background(Color, String) ->
    Style = #style{background = Color},
    set_style(Style, String).

%% Applies the given text style (or a list of text styles) to the given
%% data or style.
-spec set_text_style(text_styles(), data_or_style()) -> data_or_style().
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

%% Applies the given style to the given string data.
-spec set_style(style(), data()) -> data().
set_style(Style, String) when is_record(Style, style) ->
    case sgr(Style) of
        <<>> ->
            String;
        Seq ->
            [Seq, String, reset()]
    end.

%% Returns default style record.
-spec new_style() -> style().
new_style() ->
    #style{}.

%%
%% HELPERS.
%%

%% Constructs a SGR sequence from the given style, the given list of
%% attributes.
-spec sgr(sgr_attribute() | [sgr_attribute()] | style()) -> binary().
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

%% Generates text style attribute(s) from the given list of styles.
-spec text_style([text_style()]) -> sgr_attribute().
text_style(Styles) ->
    M = fun(bold)      -> ?TEXT_STYLE_BOLD;
           (dim)       -> ?TEXT_STYLE_DIM;
           (italic)    -> ?TEXT_STYLE_ITALIC;
           (underline) -> ?TEXT_STYLE_UNDERLINE;
           (blinking)  -> ?TEXT_STYLE_BLINKING;
           (_)         -> <<>>
        end,
    List = lists:map(M, Styles),
    join_attributes(List).

%% Generates foreground color attribute from the given color code.
-spec foreground(color()) -> sgr_attribute().
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
foreground(Byte) when is_integer(Byte), Byte >= 0, Byte =< 255 ->
    Bin = integer_to_binary(Byte),
    join_attributes([<<"38">>, <<"5">>, Bin]);
foreground([R, G, B]) when is_integer(R), R >= 0, R =< 255,
                           is_integer(G), G >= 0, G =< 255,
                           is_integer(B), B >= 0, B =< 255 ->
    [Rb, Gb, Bb] = lists:map(fun integer_to_binary/1, [R, G, B]),
    join_attributes([<<"38">>, <<"2">>, Rb, Gb, Bb]);
foreground(Hex) when is_list(Hex) ->
    RGB = hex_to_rgb(Hex),
    foreground(RGB);
foreground(_) ->
    <<>>.

%% Generates background color attribute from the given color code.
-spec background(color()) -> sgr_attribute().
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
background(Byte) when is_integer(Byte), Byte >= 0, Byte =< 255 ->
    Bin = integer_to_binary(Byte),
    join_attributes([<<"48">>, <<"5">>, Bin]);
background([R, G, B]) when is_integer(R), R >= 0, R =< 255,
                           is_integer(G), G >= 0, G =< 255,
                           is_integer(B), B >= 0, B =< 255 ->
    [Rb, Gb, Bb] = lists:map(fun integer_to_binary/1, [R, G, B]),
    join_attributes([<<"48">>, <<"2">>, Rb, Gb, Bb]);
background(Hex) when is_list(Hex) ->
    RGB = hex_to_rgb(Hex),
    background(RGB);
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

%% Converts hex to rgb.
-spec hex_to_rgb(string()) -> [byte()].
hex_to_rgb([$# | Rest]) ->
    hex_to_rgb(Rest);
hex_to_rgb([R1, R2, G1, G2, B1, B2]) ->
    R = list_to_integer([R1, R2], 16),
    G = list_to_integer([G1, G2], 16),
    B = list_to_integer([B1, B2], 16),
    [R, G, B].

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
             {?FG(black),           <<"\e[30m">>},
             {?FG(red),             <<"\e[31m">>},
             {?FG(green),           <<"\e[32m">>},
             {?FG(yellow),          <<"\e[33m">>},
             {?FG(blue),            <<"\e[34m">>},
             {?FG(magenta),         <<"\e[35m">>},
             {?FG(cyan),            <<"\e[36m">>},
             {?FG(white),           <<"\e[37m">>},
             {?FG(default),         <<"\e[39m">>},
             {?FG(unset),           <<>>},
             {?FG(0),               <<"\e[38;5;0m">>},
             {?FG(7),               <<"\e[38;5;7m">>},
             {?FG(8),               <<"\e[38;5;8m">>},
             {?FG(15),              <<"\e[38;5;15m">>},
             {?FG(16),              <<"\e[38;5;16m">>},
             {?FG(231),             <<"\e[38;5;231m">>},
             {?FG(232),             <<"\e[38;5;232m">>},
             {?FG(255),             <<"\e[38;5;255m">>},
             {?FG([0, 0, 0]),       <<"\e[38;2;0;0;0m">>},
             {?FG([255, 0, 0]),     <<"\e[38;2;255;0;0m">>},
             {?FG([0, 255, 0]),     <<"\e[38;2;0;255;0m">>},
             {?FG([0, 0, 255]),     <<"\e[38;2;0;0;255m">>},
             {?FG([255, 255, 255]), <<"\e[38;2;255;255;255m">>},
             {?FG("000000"),        <<"\e[38;2;0;0;0m">>},
             {?FG("#000000"),       <<"\e[38;2;0;0;0m">>},
             {?FG("ffffff"),        <<"\e[38;2;255;255;255m">>},
             {?FG("FFFFFF"),        <<"\e[38;2;255;255;255m">>},
             {?FG("#ffffff"),       <<"\e[38;2;255;255;255m">>},
             {?FG("#FFFFFF"),       <<"\e[38;2;255;255;255m">>},
             {?FG("f5f6f7"),        <<"\e[38;2;245;246;247m">>},
             {?FG("F5F6F7"),        <<"\e[38;2;245;246;247m">>},
             {?FG("#f5f6f7"),       <<"\e[38;2;245;246;247m">>},
             {?FG("#F5F6F7"),       <<"\e[38;2;245;246;247m">>},
             {?FG("444950"),        <<"\e[38;2;68;73;80m">>},
             {?FG("#444950"),       <<"\e[38;2;68;73;80m">>},

             %% background color
             {?BG(black),           <<"\e[40m">>},
             {?BG(red),             <<"\e[41m">>},
             {?BG(green),           <<"\e[42m">>},
             {?BG(yellow),          <<"\e[43m">>},
             {?BG(blue),            <<"\e[44m">>},
             {?BG(magenta),         <<"\e[45m">>},
             {?BG(cyan),            <<"\e[46m">>},
             {?BG(white),           <<"\e[47m">>},
             {?BG(default),         <<"\e[49m">>},
             {?BG(unset),           <<>>},
             {?BG(0),               <<"\e[48;5;0m">>},
             {?BG(7),               <<"\e[48;5;7m">>},
             {?BG(8),               <<"\e[48;5;8m">>},
             {?BG(15),              <<"\e[48;5;15m">>},
             {?BG(16),              <<"\e[48;5;16m">>},
             {?BG(231),             <<"\e[48;5;231m">>},
             {?BG(232),             <<"\e[48;5;232m">>},
             {?BG(255),             <<"\e[48;5;255m">>},
             {?BG([0, 0, 0]),       <<"\e[48;2;0;0;0m">>},
             {?BG([255, 0, 0]),     <<"\e[48;2;255;0;0m">>},
             {?BG([0, 255, 0]),     <<"\e[48;2;0;255;0m">>},
             {?BG([0, 0, 255]),     <<"\e[48;2;0;0;255m">>},
             {?BG([255, 255, 255]), <<"\e[48;2;255;255;255m">>},
             {?BG("000000"),        <<"\e[48;2;0;0;0m">>},
             {?BG("#000000"),       <<"\e[48;2;0;0;0m">>},
             {?BG("ffffff"),        <<"\e[48;2;255;255;255m">>},
             {?BG("FFFFFF"),        <<"\e[48;2;255;255;255m">>},
             {?BG("#ffffff"),       <<"\e[48;2;255;255;255m">>},
             {?BG("#FFFFFF"),       <<"\e[48;2;255;255;255m">>},
             {?BG("f5f6f7"),        <<"\e[48;2;245;246;247m">>},
             {?BG("F5F6F7"),        <<"\e[48;2;245;246;247m">>},
             {?BG("#f5f6f7"),       <<"\e[48;2;245;246;247m">>},
             {?BG("#F5F6F7"),       <<"\e[48;2;245;246;247m">>},
             {?BG("444950"),        <<"\e[48;2;68;73;80m">>},
             {?BG("#444950"),       <<"\e[48;2;68;73;80m">>},

             %% text style
             {?TS([]),                         <<>>},
             {?TS([bold]),                     <<"\e[1m">>},
             {?TS([dim]),                      <<"\e[2m">>},
             {?TS([italic]),                   <<"\e[3m">>},
             {?TS([underline]),                <<"\e[4m">>},
             {?TS([blinking]),                 <<"\e[5m">>},
             {?TS([bold, italic]),             <<"\e[1;3m">>},
             {?TS([italic, bold]),             <<"\e[3;1m">>},
             {?TS([bold, italic, underline]),  <<"\e[1;3;4m">>},

             %% normal style, fix foreground, variable background
             {?TFB([], black, black),   <<"\e[30;40m">>},
             {?TFB([], black, red),     <<"\e[30;41m">>},
             {?TFB([], black, green),   <<"\e[30;42m">>},
             {?TFB([], black, yellow),  <<"\e[30;43m">>},
             {?TFB([], black, blue),    <<"\e[30;44m">>},
             {?TFB([], black, magenta), <<"\e[30;45m">>},
             {?TFB([], black, cyan),    <<"\e[30;46m">>},
             {?TFB([], black, white),   <<"\e[30;47m">>},
             {?TFB([], black, default), <<"\e[30;49m">>},
             {?TFB([], black, unset),   <<"\e[30m">>},

             %% normal style, variable foreground, fix background
             {?TFB([], black, black),   <<"\e[30;40m">>},
             {?TFB([], red, black),     <<"\e[31;40m">>},
             {?TFB([], green, black),   <<"\e[32;40m">>},
             {?TFB([], yellow, black),  <<"\e[33;40m">>},
             {?TFB([], blue, black),    <<"\e[34;40m">>},
             {?TFB([], magenta, black), <<"\e[35;40m">>},
             {?TFB([], cyan, black),    <<"\e[36;40m">>},
             {?TFB([], white, black),   <<"\e[37;40m">>},
             {?TFB([], default, black), <<"\e[39;40m">>},
             {?TFB([], unset, black),   <<"\e[40m">>},

             %% bold style, fix foreground, variable background
             {?TFB([bold], black, black),   <<"\e[1;30;40m">>},
             {?TFB([bold], black, red),     <<"\e[1;30;41m">>},
             {?TFB([bold], black, green),   <<"\e[1;30;42m">>},
             {?TFB([bold], black, yellow),  <<"\e[1;30;43m">>},
             {?TFB([bold], black, blue),    <<"\e[1;30;44m">>},
             {?TFB([bold], black, magenta), <<"\e[1;30;45m">>},
             {?TFB([bold], black, cyan),    <<"\e[1;30;46m">>},
             {?TFB([bold], black, white),   <<"\e[1;30;47m">>},
             {?TFB([bold], black, default), <<"\e[1;30;49m">>},
             {?TFB([bold], black, unset),   <<"\e[1;30m">>},

             %% bold style, variable foreground, fix background
             {?TFB([bold], black, black),   <<"\e[1;30;40m">>},
             {?TFB([bold], red, black),     <<"\e[1;31;40m">>},
             {?TFB([bold], green, black),   <<"\e[1;32;40m">>},
             {?TFB([bold], yellow, black),  <<"\e[1;33;40m">>},
             {?TFB([bold], blue, black),    <<"\e[1;34;40m">>},
             {?TFB([bold], magenta, black), <<"\e[1;35;40m">>},
             {?TFB([bold], cyan, black),    <<"\e[1;36;40m">>},
             {?TFB([bold], white, black),   <<"\e[1;37;40m">>},
             {?TFB([bold], default, black), <<"\e[1;39;40m">>},
             {?TFB([bold], unset, black),   <<"\e[1;40m">>},

             %% bold and underline style, variable fore, fix back
             {?TFB([bold, underline], black, black),   <<"\e[1;4;30;40m">>},
             {?TFB([bold, underline], red, black),     <<"\e[1;4;31;40m">>},
             {?TFB([bold, underline], green, black),   <<"\e[1;4;32;40m">>},
             {?TFB([bold, underline], yellow, black),  <<"\e[1;4;33;40m">>},
             {?TFB([bold, underline], blue, black),    <<"\e[1;4;34;40m">>},
             {?TFB([bold, underline], magenta, black), <<"\e[1;4;35;40m">>},
             {?TFB([bold, underline], cyan, black),    <<"\e[1;4;36;40m">>},
             {?TFB([bold, underline], white, black),   <<"\e[1;4;37;40m">>},
             {?TFB([bold, underline], default, black), <<"\e[1;4;39;40m">>},
             {?TFB([bold, underline], unset, black),   <<"\e[1;4;40m">>}
            ],
    [?_assertEqual(E, sgr(I)) || {I, E} <- Tests].

reset_test() ->
    ?assertEqual(<<"\e[0m">>, reset()).

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

hex_to_rgb_test_() ->
    Tests = [
             {"000000",  [0, 0, 0]},
             {"#000000", [0, 0, 0]},

             {"ffffff",  [255, 255, 255]},
             {"FFFFFF",  [255, 255, 255]},
             {"#ffffff", [255, 255, 255]},
             {"#FFFFFF", [255, 255, 255]},

             {"f5f6f7",  [245, 246, 247]},
             {"F5F6F7",  [245, 246, 247]},
             {"#f5f6f7", [245, 246, 247]},
             {"#F5F6F7", [245, 246, 247]},

             {"00bfff",  [0, 191, 255]},
             {"00BFFF",  [0, 191, 255]},
             {"#00bfff", [0, 191, 255]},
             {"#00BFFF", [0, 191, 255]},

             {"444950",  [68, 73, 80]},
             {"#444950", [68, 73, 80]}
            ],
    [?_assertEqual(E, hex_to_rgb(I)) || {I, E} <- Tests].

-endif.
