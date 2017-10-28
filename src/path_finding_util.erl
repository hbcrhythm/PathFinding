-module(path_finding_util).

-include("path_finding.hrl").

-export([backtrace/2, expandPath/1]).

backtrace(Node, Grid) ->
	backtrace(Node, Grid, []).
backtrace(undefined, _, PathList) ->
	PathList;
backtrace(#path_finding_node{x = X, y = Y, parent = Parent}, Grid, PathList) ->
	Path = {X, Y},
	PathList2 = [Path | PathList],
	backtrace(Parent, Grid, PathList2).

expandPath(Path) ->
	Len = length(Path),
	Expanded = [],
	case Len < 2 of
		true ->
			Expanded;
		false ->
			do_expandPath(Path, Expanded)
	end.

do_expandPath([{Coord0X, Coord0Y}, {Coord1X, Coord1Y} | T], Expanded) ->
	Interpolated = interpolate(Coord0X, Coord0Y, Coord1X, Coord1Y),
	Expanded2 = Interpolated ++ Expanded,
	do_expandPath([{Coord1X, Coord1Y} | T], Expanded2);
do_expandPath([{X, Y}], Expanded) ->
	lists:reverse([{X, Y} | Expanded]).


interpolate(X0, Y0, X1, Y1) ->
	Line = [],

	Dx = abs(X1 - X0),	%%2
	Dy = abs(Y1 - Y0),	%%2

	Sx = case X0 < X1 of	%% 1
		true -> 1;
		false -> -1
	end,

	Sy = case Y0 < Y1 of	%% 1
		true -> 1;
		false -> -1
	end,

	Err = Dx - Dy,	%%0

	interpolate(X0, Y0, X1, Y1, Dx, Dy, Sx, Sy, Err, Line).

interpolate(X0, Y0, X0, Y0, _Dx, _Dy, _Sx, _Sy, _Err, Line) -> Line;
interpolate(X0, Y0, X1, Y1, Dx, Dy, Sx, Sy, Err, Line) ->
	Line2 = [{X0, Y0} | Line],
	E2 = 2 * Err,	%%0
	{Err2 ,NewX0} = case E2 > -Dy of
		true ->
			{Err - Dy, X0 + Sx};
		false ->
			{Err, X0}
	end,
	{Err3, NewY0} = case E2 < Dx of
		true ->
			{Err2 + Dx, Y0 + Sy};	
		false ->
			{Err2, Y0}
	end,
	interpolate(NewX0, NewY0, X1, Y1, Dx, Dy, Sx, Sy, Err3, Line2).



