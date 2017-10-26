
-record(grid, {
		width
		,height
		,nodes
	}).

-record(node, {
		x
		,y
		,walkable = true
	}).

%% @doc DiagonalMovement
-define(DM_Always, 1).
-define(DM_Never, 2).
-define(DM_IfAtMostOneObstacle, 3).
-define(DM_OnlyWhenNoObstacles, 4).