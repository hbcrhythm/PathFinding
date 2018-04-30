-module(path_finding_util).

-include("path_finding.hrl").

-export([backtrace/2, expandPath/1, smoothenPath/2]).

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
	Expanded2 = Interpolated ++ Expanded ,
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


smoothenPath(Path = [{X0, Y0}, {X1, Y1} | Path2 ], Grid) ->
	{XL, YL} = lists:last(Path),
	
	Sx = X0,
	Sy = Y0,
	NewPath = [{Sx, Sy}],

	F = fun({Ex, Ey}, {SxAcc, SyAcc, LastValidCoord = {LastValidCoordX, LastValidCoordY}, NewPathAcc}) ->

		_Line = [_ | Line2] = lists:reverse(interpolate(SxAcc, SyAcc, Ex, Ey)),
		Blocked = smoothenPath_blocked(Line2, Grid),
		case Blocked of
			true ->
				NewPathAcc2 = [LastValidCoord | NewPathAcc],
				{LastValidCoordX, LastValidCoordY, {Ex, Ey}, NewPathAcc2};
			false ->
				{SxAcc, SyAcc, {Ex, Ey}, NewPathAcc}
		end
	end,

	{_, _, _, NewPath2} = lists:foldl(F, {Sx, Sy, {X1, Y1}, NewPath}, Path2),
	lists:reverse([{XL, YL} |NewPath2]).


smoothenPath_blocked([], _) -> false;
smoothenPath_blocked([{TestCoord0, TestCoord1} | T], Grid) ->
	case path_finding_grid:isWalkableAt(TestCoord0, TestCoord1, Grid) of
			true ->
				smoothenPath_blocked(T, Grid);
			false ->
				true
	end.










