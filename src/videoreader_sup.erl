-module(videoreader_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-version(1.0).

-behaviour(supervisor).

-export([init/1,start_link/0]).

-export([start_reader/3]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_reader(Host, Name, Path) ->
  supervisor:start_child(video_reader_sup, [Host, Name, Path]).

init([video_reader]) ->
    {ok,
        {{simple_one_for_one, 3, 10},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {video_reader,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [video_reader]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };

init([]) ->
  Supervisors = [
    {   videoreader_event_sup,                         % Id       = internal id
        {videoreader_event,start_link,[]},             % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [videoreader_event]                               % Modules  = [Module] | dynamic
    },
    {   video_reader_sup,
        {supervisor,start_link,[{local, video_reader_sup}, ?MODULE, [video_reader]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    }
  ],
  
  {ok, {{one_for_one, 3, 10}, Supervisors}}.
