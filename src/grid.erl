-module(grid).

-include("path_finding.hrl").

-export([grid/2, isInside/3, setWalkAbleAt/4]).

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

