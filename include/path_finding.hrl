
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