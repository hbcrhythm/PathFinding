
-record(path_finding_grid, {
		width
		,height
		,nodes
	}).

-record(path_finding_node, {
		x
		,y
		,walkable = true
		,g = 0
		,h = 0
		,f = 0
		,parent
		,opened = false
		,closed = false
		,tested
	}).

-define(Heuristic_Manhattan, fun(Dx, Dy) -> Dx + Dy end).
-define(Heuristic_Euclidean, fun(Dx, Dy) -> math:sqrt(Dx * Dx + Dy * Dy) end).
-define(Heuristic_Octile, fun(Dx, Dy) -> case Dx < Dy of true ->(math:sqrt(2) - 1) * Dx + Dy; false ->(math:sqrt(2) - 1) * Dy + Dx end end).
-define(Heuristic_Chebyshev, fun(Dx, Dy) -> max(Dx, Dy) end).

%% @doc DiagonalMovement
-define(DM_Always, 1).
-define(DM_Never, 2).
-define(DM_IfAtMostOneObstacle, 3).
-define(DM_OnlyWhenNoObstacles, 4).

-record(path_finding_finder, {
		diagonal_movement = ?DM_IfAtMostOneObstacle
		,heuristic = ?Heuristic_Manhattan
		,track_jump_recursion = false
	}).

