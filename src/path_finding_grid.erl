-module(path_finding_grid).

-include("path_finding.hrl").

-export([path_finding_grid/2, isInside/3, setWalkableAt/4, isWalkableAt/3, getNodeAt/3, getNeighbors/3, updateNodeAt/4]).

path_finding_grid(Width, Height) ->
	#path_finding_grid{
		width = Width
		,height = Height
		,nodes = buildNodes(Width, Height)
	}.

buildNodes(Width, Height) when is_integer(Width) andalso is_integer(Height) ->
	F = fun(Key = {I, J}, Acc) ->
		Node = #path_finding_node{x = J, y = I},
		gb_trees:enter(Key, Node, Acc)
	end,
	lists:foldl(F, gb_trees:empty(), [{I,J} || I <- lists:seq(0, Height - 1), J <- lists:seq(0, Width - 1)]).

isInside(X, Y, #path_finding_grid{width = Width, height = Height}) ->
	(X >= 0 andalso X < Width) andalso (Y >= 0 andalso Y < Height).

setWalkableAt(X, Y, WalkAble, Grid = #path_finding_grid{nodes = Nodes}) ->
	case gb_trees:lookup({Y, X}, Nodes) of
		none ->
			Grid;
		{value, Node} ->
			Node2 = Node#path_finding_node{walkable = WalkAble},
			Nodes2 = gb_trees:enter({Y, X}, Node2, Nodes),
			Grid#path_finding_grid{nodes = Nodes2}
	end.

isWalkableAt(X, Y, Grid = #path_finding_grid{nodes = Nodes}) ->
	isInside(X, Y, Grid) andalso begin
		case gb_trees:lookup({Y, X}, Nodes) of
			none ->
				false;
			{value, #path_finding_node{walkable = Walkable}} ->
				Walkable
		end
	end.

getNodeAt(X, Y, #path_finding_grid{nodes = Nodes}) ->
	case gb_trees:lookup({Y, X}, Nodes) of
		none ->
			#path_finding_node{};
		{value, Node} ->
			Node
	end.

updateNodeAt(X, Y, Node, Grid = #path_finding_grid{nodes = Nodes}) ->
	Key = {Y, X},
	case gb_trees:lookup(Key, Nodes) of
		none ->
			Grid;
		{value, _Node} ->
			Nodes2 = gb_trees:update(Key, Node, Nodes),
			Grid#path_finding_grid{nodes = Nodes2}
	end.

%% @doc
%% Get the neighbors of the given node.
%%
%%     offsets      diagonalOffsets:
%%  +---+---+---+    +---+---+---+
%%  |   | 0 |   |    | 0 |   | 1 |
%%  +---+---+---+    +---+---+---+
%%  | 3 |   | 1 |    |   |   |   |
%%  +---+---+---+    +---+---+---+
%%  |   | 2 |   |    | 3 |   | 2 |
%%  +---+---+---+    +---+---+---+
%%
%%  When allowDiagonal is true, if offsets[i] is valid, then
%%  diagonalOffsets[i] and
%%  diagonalOffsets[(i + 1) % 4] is valid.
%% @end
getNeighbors(#path_finding_node{x = X, y = Y}, DianonalMovement, Grid = #path_finding_grid{nodes = Nodes}) ->
	Neighbors = [],

	%% ↑
	{S0, Neighbors2} = case isWalkableAt(X, Y - 1, Grid) of
		true ->
			{value, Node2} = gb_trees:lookup({Y - 1, X}, Nodes),
			{true, [Node2 | Neighbors]};
		false ->
			{false, Neighbors}
	end,

	%% →
	{S1, Neighbors3} = case	isWalkableAt(X + 1, Y, Grid) of
		true ->
			{value, Node3} = gb_trees:lookup({Y, X + 1}, Nodes),
			{true, [Node3 | Neighbors2]};
		false ->
			{false, Neighbors2}
	end,

	%% ↓
	{S2, Neighbors4} = case isWalkableAt(X, Y + 1, Grid) of
		true ->
			{value, Node4} = gb_trees:lookup({Y + 1, X}, Nodes),
			{true, [Node4 | Neighbors3]};
		false ->
			{false, Neighbors3}
	end,

	%% ←
	{S3, Neighbors5} = case isWalkableAt(X - 1, Y, Grid) of
		true ->
			{value, Node5} = gb_trees:lookup({Y, X - 1}, Nodes),
			{true, [Node5 | Neighbors4]};
		false ->
			{false, Neighbors4}
	end,

	if
		DianonalMovement == ?DM_Never ->
			lists:reverse(Neighbors5);
		true ->
			{D0, D1, D2, D3} = if
				DianonalMovement == ?DM_OnlyWhenNoObstacles  ->
				%% 都不是阻碍点
					{S3 andalso S0,
					S0 andalso S1,
					S1 andalso S2,
					S2 andalso S3};
				DianonalMovement == ?DM_IfAtMostOneObstacle ->
				%% 至少一个不是阻碍点
					{S3 orelse S0,
					S0 orelse S1,
					S1 orelse S2,
					S2 orelse S3};
				DianonalMovement == ?DM_Always ->
					{true, true, true, true}
			end,
			%% ↖
			Neighbors6 = case D0 andalso isWalkableAt(X - 1, Y - 1, Grid) of
				true ->
					{value, Node6} = gb_trees:lookup({Y - 1, X - 1}, Nodes),
					[Node6 | Neighbors5];
				false ->
					Neighbors5
			end,
			%% ↗
			Neighbors7 = case D1 andalso isWalkableAt(X + 1, Y - 1, Grid) of
				true ->
					{value, Node7} = gb_trees:lookup({Y - 1, X + 1}, Nodes),
					[Node7 | Neighbors6];
				false ->
					Neighbors6
			end,
			%% ↘
			Neighbors8 = case D2 andalso isWalkableAt(X + 1, Y + 1, Grid) of
				true ->
					{value, Node8} = gb_trees:lookup({Y + 1, X + 1}, Nodes),
					[Node8 | Neighbors7];
				false ->
					Neighbors7
			end,
			%% ↙
			Neighbors9 = case D3 andalso isWalkableAt(X - 1, Y + 1, Grid) of
				true ->
					{value, Node9} = gb_trees:lookup({Y + 1, X - 1}, Nodes),
					[Node9 | Neighbors8];
				false ->
					Neighbors8
			end,
			lists:reverse(Neighbors9)
	end.


