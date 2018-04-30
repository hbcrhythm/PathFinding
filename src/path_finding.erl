-module(path_finding).
-include("path_finding.hrl").
%% API exports
-export([grid/2, jumpPoinmtFinder/6, test/0]).

%%====================================================================
%% API functions
%%====================================================================

grid(Width, Height) ->
	path_finding_grid:path_finding_grid(Width, Height).

jumpPoinmtFinder(StartX, StartY, EndX, EndY, Finder, Grid) ->
	jump_point_finder:finder(StartX, StartY, EndX, EndY, Finder, Grid).

test() ->
	application:ensure_started(path_finding),
	Grid = path_finding_grid:path_finding_grid(11, 11),
	Grid2 = path_finding_grid:setWalkableAt(2, 2 ,false, Grid),
	Grid3 = path_finding_grid:setWalkableAt(1, 2 ,false, Grid2),
	Grid4 = path_finding_grid:setWalkableAt(3, 2 ,false, Grid3),
	Grid5 = path_finding_grid:setWalkableAt(5, 4 ,false, Grid4),
	Grid6 = path_finding_grid:setWalkableAt(8, 8 ,false, Grid5),
	Grid7 = path_finding_grid:setWalkableAt(9, 9 ,false, Grid6),
	Grid8 = path_finding_grid:setWalkableAt(8, 9 ,false, Grid7),
	% Grid9 = path_finding_grid:setWalkableAt(10, 10 ,false, Grid8),
	Path = jump_point_finder:finder(1, 1, 10, 10, #path_finding_finder{diagonal_movement = ?DM_Never, heuristic = ?Heuristic_Euclidean}, Grid8),
	path_finding_util:smoothenPath(Path, Grid8).

%%====================================================================
%% Internal functions
%%===================application:ensure_all_started(path_finding).=================================================
