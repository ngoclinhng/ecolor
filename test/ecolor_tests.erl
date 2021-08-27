-module(ecolor_tests).
-include_lib("eunit/include/eunit.hrl").

-define(IODATA, [<<"t">>, "e", 115, [[<<"t">>]]]).

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
         {unset,   "unset",   "unset"},

         %% 8-bit integer color
         {0,   "test", "\e[38;5;0mtest\e[0m"},
         {7,   "test", "\e[38;5;7mtest\e[0m"},
         {8,   "test", "\e[38;5;8mtest\e[0m"},
         {15,  "test", "\e[38;5;15mtest\e[0m"},
         {16,  "test", "\e[38;5;16mtest\e[0m"},
         {231, "test", "\e[38;5;231mtest\e[0m"},
         {232, "test", "\e[38;5;232mtest\e[0m"},
         {255, "test", "\e[38;5;255mtest\e[0m"},
         {-1,  "test", "test"},
         {256, "test", "test"},

         %% 24-bit rgb
         {[0, 0, 0],       "test", "\e[38;2;0;0;0mtest\e[0m"},
         {[255, 0, 0],     "test", "\e[38;2;255;0;0mtest\e[0m"},
         {[0, 255, 0],     "test", "\e[38;2;0;255;0mtest\e[0m"},
         {[0, 0, 255],     "test", "\e[38;2;0;0;255mtest\e[0m"},
         {[255, 255, 255], "test", "\e[38;2;255;255;255mtest\e[0m"},

         %% hex
         {"000000",  "hex", "\e[38;2;0;0;0mhex\e[0m"},
         {"#000000", "hex", "\e[38;2;0;0;0mhex\e[0m"},
         {"ffffff",  "hex", "\e[38;2;255;255;255mhex\e[0m"},
         {"FFFFFF",  "hex", "\e[38;2;255;255;255mhex\e[0m"},
         {"#ffffff", "hex", "\e[38;2;255;255;255mhex\e[0m"},
         {"#FFFFFF", "hex", "\e[38;2;255;255;255mhex\e[0m"},
         {"f5f6f7",  "hex", "\e[38;2;245;246;247mhex\e[0m"},
         {"F5F6F7",  "hex", "\e[38;2;245;246;247mhex\e[0m"},
         {"#f5f6f7", "hex", "\e[38;2;245;246;247mhex\e[0m"},
         {"#F5F6F7", "hex", "\e[38;2;245;246;247mhex\e[0m"},
         {"444950",  "hex", "\e[38;2;68;73;80mhex\e[0m"},
         {"#444950", "hex", "\e[38;2;68;73;80mhex\e[0m"}
        ],
    [?_assertEqual(E, to_s(ecolor:set_foreground(C, S))) || {C, S, E} <- T].

set_foreground_for_iodata_test_() ->
    T = [
         {black,   "\e[30mtest\e[0m"},
         {red,     "\e[31mtest\e[0m"},
         {green,   "\e[32mtest\e[0m"},
         {yellow,  "\e[33mtest\e[0m"},
         {blue,    "\e[34mtest\e[0m"},
         {magenta, "\e[35mtest\e[0m"},
         {cyan,    "\e[36mtest\e[0m"},
         {white,   "\e[37mtest\e[0m"},
         {default, "\e[39mtest\e[0m"},
         {unset,   "test"},

         %% 8-bit integer color
         {0,   "\e[38;5;0mtest\e[0m"},
         {7,   "\e[38;5;7mtest\e[0m"},
         {8,   "\e[38;5;8mtest\e[0m"},
         {15,  "\e[38;5;15mtest\e[0m"},
         {16,  "\e[38;5;16mtest\e[0m"},
         {231, "\e[38;5;231mtest\e[0m"},
         {232, "\e[38;5;232mtest\e[0m"},
         {255, "\e[38;5;255mtest\e[0m"},
         {-1,  "test"},
         {256, "test"},

         %% 24-bit rgb
         {[0, 0, 0],      "\e[38;2;0;0;0mtest\e[0m"},
         {[255, 0, 0],     "\e[38;2;255;0;0mtest\e[0m"},
         {[0, 255, 0],     "\e[38;2;0;255;0mtest\e[0m"},
         {[0, 0, 255],     "\e[38;2;0;0;255mtest\e[0m"},
         {[255, 255, 255], "\e[38;2;255;255;255mtest\e[0m"},

         %% hex
         {"000000",  "\e[38;2;0;0;0mtest\e[0m"},
         {"#000000", "\e[38;2;0;0;0mtest\e[0m"},
         {"ffffff",  "\e[38;2;255;255;255mtest\e[0m"},
         {"FFFFFF",  "\e[38;2;255;255;255mtest\e[0m"},
         {"#ffffff", "\e[38;2;255;255;255mtest\e[0m"},
         {"#FFFFFF", "\e[38;2;255;255;255mtest\e[0m"},
         {"f5f6f7",  "\e[38;2;245;246;247mtest\e[0m"},
         {"F5F6F7",  "\e[38;2;245;246;247mtest\e[0m"},
         {"#f5f6f7", "\e[38;2;245;246;247mtest\e[0m"},
         {"#F5F6F7", "\e[38;2;245;246;247mtest\e[0m"},
         {"444950",  "\e[38;2;68;73;80mtest\e[0m"},
         {"#444950", "\e[38;2;68;73;80mtest\e[0m"}
        ],
    [?_assertEqual(E, to_s(ecolor:set_foreground(C, ?IODATA)))
     || {C, E} <- T].

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
         {unset,   "unset",   "unset"},

         %% 8-bit integer color
         {0,   "test", "\e[48;5;0mtest\e[0m"},
         {7,   "test", "\e[48;5;7mtest\e[0m"},
         {8,   "test", "\e[48;5;8mtest\e[0m"},
         {15,  "test", "\e[48;5;15mtest\e[0m"},
         {16,  "test", "\e[48;5;16mtest\e[0m"},
         {231, "test", "\e[48;5;231mtest\e[0m"},
         {232, "test", "\e[48;5;232mtest\e[0m"},
         {255, "test", "\e[48;5;255mtest\e[0m"},
         {-1,  "test", "test"},
         {256, "test", "test"},

         %% 24-bit rgb
         {[0, 0, 0],       "test", "\e[48;2;0;0;0mtest\e[0m"},
         {[255, 0, 0],     "test", "\e[48;2;255;0;0mtest\e[0m"},
         {[0, 255, 0],     "test", "\e[48;2;0;255;0mtest\e[0m"},
         {[0, 0, 255],     "test", "\e[48;2;0;0;255mtest\e[0m"},
         {[255, 255, 255], "test", "\e[48;2;255;255;255mtest\e[0m"},

         %% hex
         {"000000",  "hex", "\e[48;2;0;0;0mhex\e[0m"},
         {"#000000", "hex", "\e[48;2;0;0;0mhex\e[0m"},
         {"ffffff",  "hex", "\e[48;2;255;255;255mhex\e[0m"},
         {"FFFFFF",  "hex", "\e[48;2;255;255;255mhex\e[0m"},
         {"#ffffff", "hex", "\e[48;2;255;255;255mhex\e[0m"},
         {"#FFFFFF", "hex", "\e[48;2;255;255;255mhex\e[0m"},
         {"f5f6f7",  "hex", "\e[48;2;245;246;247mhex\e[0m"},
         {"F5F6F7",  "hex", "\e[48;2;245;246;247mhex\e[0m"},
         {"#f5f6f7", "hex", "\e[48;2;245;246;247mhex\e[0m"},
         {"#F5F6F7", "hex", "\e[48;2;245;246;247mhex\e[0m"},
         {"444950",  "hex", "\e[48;2;68;73;80mhex\e[0m"},
         {"#444950", "hex", "\e[48;2;68;73;80mhex\e[0m"}
        ],
    [?_assertEqual(E, to_s(ecolor:set_background(C, S))) || {C, S, E} <- T].

set_background_for_iodata_test_() ->
    T = [
         {black,   "\e[40mtest\e[0m"},
         {red,     "\e[41mtest\e[0m"},
         {green,   "\e[42mtest\e[0m"},
         {yellow,  "\e[43mtest\e[0m"},
         {blue,    "\e[44mtest\e[0m"},
         {magenta, "\e[45mtest\e[0m"},
         {cyan,    "\e[46mtest\e[0m"},
         {white,   "\e[47mtest\e[0m"},
         {default, "\e[49mtest\e[0m"},
         {unset,   "test"},

         %% 8-bit integer color
         {0,   "\e[48;5;0mtest\e[0m"},
         {7,   "\e[48;5;7mtest\e[0m"},
         {8,   "\e[48;5;8mtest\e[0m"},
         {15,  "\e[48;5;15mtest\e[0m"},
         {16,  "\e[48;5;16mtest\e[0m"},
         {231, "\e[48;5;231mtest\e[0m"},
         {232, "\e[48;5;232mtest\e[0m"},
         {255, "\e[48;5;255mtest\e[0m"},
         {-1,  "test"},
         {256, "test"},

         %% 24-bit rgb
         {[0, 0, 0],       "\e[48;2;0;0;0mtest\e[0m"},
         {[255, 0, 0],     "\e[48;2;255;0;0mtest\e[0m"},
         {[0, 255, 0],     "\e[48;2;0;255;0mtest\e[0m"},
         {[0, 0, 255],     "\e[48;2;0;0;255mtest\e[0m"},
         {[255, 255, 255], "\e[48;2;255;255;255mtest\e[0m"},

         %% hex
         {"000000",  "\e[48;2;0;0;0mtest\e[0m"},
         {"#000000", "\e[48;2;0;0;0mtest\e[0m"},
         {"ffffff",  "\e[48;2;255;255;255mtest\e[0m"},
         {"FFFFFF",  "\e[48;2;255;255;255mtest\e[0m"},
         {"#ffffff", "\e[48;2;255;255;255mtest\e[0m"},
         {"#FFFFFF", "\e[48;2;255;255;255mtest\e[0m"},
         {"f5f6f7",  "\e[48;2;245;246;247mtest\e[0m"},
         {"F5F6F7",  "\e[48;2;245;246;247mtest\e[0m"},
         {"#f5f6f7", "\e[48;2;245;246;247mtest\e[0m"},
         {"#F5F6F7", "\e[48;2;245;246;247mtest\e[0m"},
         {"444950",  "\e[48;2;68;73;80mtest\e[0m"},
         {"#444950", "\e[48;2;68;73;80mtest\e[0m"}
        ],
    [?_assertEqual(E, to_s(ecolor:set_background(C, ?IODATA)))
     || {C, E} <- T].

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
