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

%% Constructs a SGR sequence from the given style or the given list of
%% attributes.
-spec sgr(binary() | [binary()] | style()) -> binary().
sgr(Style) ->
    ok.

%% Returns the SGR reset sequence to reset all attributes to their
%% defaults.
-spec reset() -> binary().
reset() ->
    sgr(?RESET_CODE).

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
