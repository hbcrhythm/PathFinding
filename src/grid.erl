-module(grid).

-include("path_finding.hrl").

-export([grid/2, isInside/3, setWalkAbleAt/4, getNeighbors/3]).

grid(Width, Height) ->
	#grid{
		width = Width
		,height = Height
		,nodes = buildNodes(Width, Height)
	}.

buildNodes(Width, Height) when is_integer(Width) andalso is_integer(Height) ->
	F = fun(Key = {X, Y}, Acc) ->
		Node = #node{x = X, y = Y},
		gb_trees:enter(Key, Node, Acc)
	end,
	lists:foldl(F, gb_trees:empty(), [{X,Y} ||X <- lists:seq(1, Width), Y <- lists:seq(1, Height)]).

isInside(#grid{width = Width, height = Height}, X, Y) ->
	(X >= 0 andalso X < Width) andalso (Y >= 0 andalso Y < Height).

setWalkAbleAt(Grid = #grid{nodes = Nodes}, X, Y, WalkAble) ->
	case gb_trees:lookup({X, Y}, Nodes) of
		none ->
			Grid;
		{value, Node} ->
			Node2 = Node#node{walkable = WalkAble},
			Nodes2 = gb_trees:enter({X, Y}, Node2, Nodes),
			Grid#grid{nodes = Nodes2}
	end.

isWalkableAt(Grid = #grid{nodes = Nodes}, X, Y) ->
	isInside(Grid, X, Y) andalso begin
		case gb_trees:lookup({X, Y}, Nodes) of
			none ->
				false;
			{value, #node{walkable = WalkAble}} ->
				WalkAble
		end
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
getNeighbors(Grid = #grid{nodes = Nodes}, #node{x = X, y = Y}, DianomalMovement) ->
	Neighbors = [],

	%% ↑
	{S0, Neighbors2} = case isWalkableAt(Grid, X, Y - 1) of
		true ->
			{value, Node2} = gb_trees:lookup({Y - 1, X}, Nodes),
			{true, [Node2 | Neighbors]};
		false ->
			{false, Neighbors}
	end,

	%% →
	{S1, Neighbors3} = case	isWalkableAt(Grid, X + 1, Y) of
		true ->
			{value, Node3} = gb_trees:lookup({Y, X + 1}, Nodes),
			{true, [Node3 | Neighbors2]};
		false ->
			{false, Neighbors2}
	end,

	%% ↓
	{S2, Neighbors4} = case isWalkableAt(Grid, X, Y + 1) of
		true ->
			{value, Node4} = gb_trees:lookup({Y + 1, X}, Nodes),
			{true, [Node4 | Neighbors3]};
		false ->
			{false, Neighbors3}
	end,

	%% ←
	{S3, Neighbors5} = case isWalkableAt(Grid, X - 1, Y) of
		true ->
			{value, Node5} = gb_trees:lookup({Y, X - 1}, Nodes),
			{true, [Node5 | Neighbors4]};
		false ->
			{false, Neighbors4}
	end,

	if
		DianomalMovement == ?DM_Never ->
			Neighbors5;
		true ->
			{D0, D1, D2, D3} = if
				DianomalMovement == ?DM_OnlyWhenNoObstacles  ->
				%% 都不是阻碍点
					{S3 andalso S0,
					S0 andalso S1,
					S1 andalso S2,
					S2 andalso S3};
				DianomalMovement == ?DM_IfAtMostOneObstacle ->
				%% 至少一个不是阻碍点
					{S3 orelse S0,
					S0 orelse S1,
					S1 orelse S2,
					S2 orelse S3};
				DianomalMovement == ?DM_Always ->
					{true, true, true, true}
			end,
			%% ↖
			Neighbors6 = case D0 andalso isWalkableAt(Grid, X - 1, Y - 1) of
				true ->
					{value, Node6} = gb_trees:lookup({Y - 1, X - 1}, Nodes),
					[Node6 | Neighbors5];
				false ->
					Neighbors5
			end,
			%% ↗
			Neighbors7 = case D1 andalso isWalkableAt(Grid, X + 1, Y - 1) of
				true ->
					{value, Node7} = gb_trees:lookup({Y - 1, X + 1}, Nodes),
					[Node7 | Neighbors6];
				false ->
					Neighbors6
			end,
			%% ↘
			Neighbors8 = case D2 andalso isWalkableAt(Grid, X + 1, Y + 1) of
				true ->
					{value, Node8} = gb_trees:lookup({Y + 1, X + 1}, Nodes),
					[Node8 | Neighbors7];
				false ->
					Neighbors7
			end,
			%% ↙
			Neighbors9 = case D3 andalso isWalkableAt(Grid, X - 1, Y + 1) of
				true ->
					{value, Node9} = gb_trees:lookup({Y + 1, X - 1}, Nodes),
					[Node9 | Neighbors8];
				false ->
					Neighbors8
			end,
			Neighbors9
	end.


