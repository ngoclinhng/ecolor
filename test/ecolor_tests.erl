-module(ecolor_tests).
-include_lib("eunit/include/eunit.hrl").

%% -define(TEST_STRING, "test").
%% -define(TEST_IODATA, [<<"t">>, "e", 115, [[<<"t">>]]]).

set_foreground_for_string_test_() ->
    T = [
         {black,   "black",   "\e[30mblack\e[0m"},
         {red,     "red",     "\e[31mred\e[0m"},
         {green,   "green",   "\e[32mgreen\e[0m"},
         {yellow,  "yellow",  "\e[33myellow\e[0m"},
         {blue,    "blue",    "\e[34mblue\e[0m"},
         {magenta, "magenta", "\e[35mmagenta\e[0m"},
         {cyan,    "cyan",    "\e[36mcyan\e[0m"},
         {white,   "white",   "\e[37mwhite\e[0m"},
         {default, "default", "\e[39mdefault\e[0m"},
         {unset,   "unset",   "unset"}
        ],
    [?_assertEqual(E, to_s(ecolor:set_foreground(C, S))) || {C, S, E} <- T].

set_background_for_string_test_() ->
    T = [
         {black,   "black",   "\e[40mblack\e[0m"},
         {red,     "red",     "\e[41mred\e[0m"},
         {green,   "green",   "\e[42mgreen\e[0m"},
         {yellow,  "yellow",  "\e[43myellow\e[0m"},
         {blue,    "blue",    "\e[44mblue\e[0m"},
         {magenta, "magenta", "\e[45mmagenta\e[0m"},
         {cyan,    "cyan",    "\e[46mcyan\e[0m"},
         {white,   "white",   "\e[47mwhite\e[0m"},
         {default, "default", "\e[49mdefault\e[0m"},
         {unset,   "unset",   "unset"}
        ],
    [?_assertEqual(E, to_s(ecolor:set_background(C, S))) || {C, S, E} <- T].

set_text_style_for_string_test_() ->
    T = [
         %% individual style
         {bold,        "bold",      "\e[1mbold\e[0m"},
         {[bold],      "bold",      "\e[1mbold\e[0m"},
         {dim,         "dim",       "\e[2mdim\e[0m"},
         {[dim],       "dim",       "\e[2mdim\e[0m"},
         {italic,      "italic",    "\e[3mitalic\e[0m"},
         {[italic],    "italic",    "\e[3mitalic\e[0m"},
         {underline,   "underline", "\e[4munderline\e[0m"},
         {[underline], "underline", "\e[4munderline\e[0m"},
         {blinking,    "blinking",  "\e[5mblinking\e[0m"},
         {[blinking],  "blinking",  "\e[5mblinking\e[0m"},

         %% mix styles
         {[bold, italic], "bold and italic", "\e[1;3mbold and italic\e[0m"},
         {
          [italic, underline], "italic and underline",
          "\e[3;4mitalic and underline\e[0m"
         }
        ],
    [?_assertEqual(E, to_s(ecolor:set_text_style(A, S))) || {A, S, E} <- T].

%%
%% HELPERS.
%%

-spec to_s(iolist() | binary()) -> string().
to_s(IOData) ->
    Bin = iolist_to_binary(IOData),
    binary_to_list(Bin).
