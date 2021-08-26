%% Control Sequence Introducer (CSI): ESC [.
-define(CSI, <<27,91>>).

%%
%% Select Graphic Rendition (SGR) attributes.
%% See [1] and [2] for more details.
%%
%% [1] - https://man7.org/linux/man-pages/man4/console_codes.4.html
%% [2] - https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
%%

-define(ATTRIBUTE_SEPARATOR, <<";">>).
-define(RESET_CODE, <<"0">>).

%% The only universally supported (?) mode is bold, other modes may not
%% be supported by some terminals. (See [2]).
-define(GRAPHIC_MODE_BOLD,      <<"1">>).
-define(GRAPHIC_MODE_DIM,       <<"2">>).
-define(GRAPHIC_MODE_ITALIC,    <<"3">>).
-define(GRAPHIC_MODE_UNDERLINE, <<"4">>).
-define(GRAPHIC_MODE_BLINKING,  <<"5">>).

%% Common foreground colors
-define(FOREGROUND_BLACK,   <<"30">>).
-define(FOREGROUND_RED,     <<"31">>).
-define(FOREGROUND_GREEN,   <<"32">>).
-define(FOREGROUND_YELLOW,  <<"33">>).
-define(FOREGROUND_BLUE,    <<"34">>).
-define(FOREGROUND_MAGENTA, <<"35">>).
-define(FOREGROUND_CYAN,    <<"36">>).
-define(FOREGROUND_WHITE,   <<"37">>).
-define(FOREGROUND_DEFAULT, <<"39">>).

%% Common background colors.
-define(BACKGROUND_BLACK,   <<"40">>).
-define(BACKGROUND_RED,     <<"41">>).
-define(BACKGROUND_GREEN,   <<"42">>).
-define(BACKGROUND_YELLOW,  <<"43">>).
-define(BACKGROUND_BLUE,    <<"44">>).
-define(BACKGROUND_MAGENTA, <<"45">>).
-define(BACKGROUND_CYAN,    <<"46">>).
-define(BACKGROUND_WHITE,   <<"47">>).
-define(BACKGROUND_DEFAULT, <<"49">>).
