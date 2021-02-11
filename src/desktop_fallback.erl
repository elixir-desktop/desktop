-module(desktop_fallback).
-include_lib("wx/include/wx.hrl").
-export([notification_events_available/0]).

notification_events_available() ->
    Version = {?wxMAJOR_VERSION, ?wxMINOR_VERSION, ?wxRELEASE_NUMBER},
    case Version of
        {Major, Minor, _} when Major >= 3, Minor >= 1 -> true;
        _ -> false
    end
.