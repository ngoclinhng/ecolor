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

-record(style, {text_styles = []    :: [text_style()],
                foreground  = unset :: color(),
                background  = unset :: color()
               }).

-type style() :: #style{}.
-type sgr_attribute() :: binary().

%%
%% HELPERS.
%%

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
