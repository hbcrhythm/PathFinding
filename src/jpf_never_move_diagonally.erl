-module(jpf_never_move_diagonally).

-include("path_finding.hrl").

-export([findNeighbors/2, jump/7]).

findNeighbors(Node = #path_finding_node{parent = undefined}, Grid) ->
	NeighborNodes = path_finding_grid:getNeighbors(Node, ?DM_Never, Grid),
	% lager:info("NeighborNodes ~w",[NeighborNodes]),
	Neighbors = [{NeighborNodex, NeighborNodeY} || #path_finding_node{x = NeighborNodex, y = NeighborNodeY} <- NeighborNodes],
	Neighbors;
findNeighbors(#path_finding_node{x = X, y = Y, parent = #path_finding_node{x = Px, y = Py}}, Grid) ->
	Neighbors = [],
	Dx = trunc((X - Px) / max(abs(X - Px), 1)),
	Dy = trunc((Y - Py) / max(abs(Y - Py), 1)),
	if
		Dx =/= 0 ->
			%% search horizontally/vertically
			Neighbors2 = case path_finding_grid:isWalkableAt(X, Y - 1, Grid) of
				true ->
					[{X, Y - 1} | Neighbors];
				false ->
					Neighbors
			end,
			Neighbors3 = case path_finding_grid:isWalkableAt(X, Y + 1, Grid) of
				true ->
					[{X, Y + 1} | Neighbors2];
				false ->
					Neighbors2
			end,
			Neighbors4 = case path_finding_grid:isWalkableAt(X + Dx, Y, Grid) of
				true ->
					[{X + Dx, Y} | Neighbors3];
				false ->
					Neighbors3
			end,
			Neighbors4;
		Dy =/= 0 ->
			Neighbors2 = case path_finding_grid:isWalkableAt(X - 1, Y, Grid) of
				true ->
					[{X - 1, Y} | Neighbors];
				false ->
					Neighbors
			end,
			Neighbors3 = case path_finding_grid:isWalkableAt(X + 1, Y, Grid) of
				true ->
					[{X + 1, Y} | Neighbors2];
				false ->
					Neighbors2
			end,
			Neighbors4 = case path_finding_grid:isWalkableAt(X, Y + Dy, Grid) of
				true ->
					[{X , Y + Dy} | Neighbors3];
				false ->
					Neighbors3
			end,
			Neighbors4;
		true ->
			Neighbors
	end.

% jump(X, Y, _, _, #path_finding_node{x = X, y = Y}, _, Grid) ->
% 	{{X, Y}, Grid};
jump(X, Y, Px, Py, EndNode = #path_finding_node{x = EndX, y = EndY}, Finder = #path_finding_finder{track_jump_recursion = TrackJumpRecursion}, Grid) ->
	lager:info("check jump ~w ~w ~w ~w ~n",[X, Y, Px, Py]),
	Dx = X - Px,
	Dy = Y - Py,
	case path_finding_grid:isWalkableAt(X, Y, Grid) of
		true ->
			Grid3 = case TrackJumpRecursion of
				true ->
					Node = path_finding_grid:getNodeAt(X, Y, Grid),
					Node2 = Node#path_finding_node{tested = true},
					Grid2 = path_finding_grid:updateNodeAt(X, Y, Node2, Grid),
					Grid2;
				false ->
					Grid
			end,
			case X =:= EndX andalso Y =:= EndY of
				true ->
					{{X, Y}, Grid3};
				false ->
					Result = if
						Dx =/= 0 -> %%horizontally, moveing along x
							case (path_finding_grid:isWalkableAt(X , Y - 1, Grid3) andalso 
									not path_finding_grid:isWalkableAt(X - Dx, Y - 1, Grid3)) orelse
									(path_finding_grid:isWalkableAt(X, Y + 1, Grid3) andalso 
									not path_finding_grid:isWalkableAt(X - Dx, Y + 1, Grid3)) of
								true ->
									{{X, Y}, Grid3};
								false ->
									undefined
							end;
						Dy =/= 0 -> %%vertically, moveing along y
							case (path_finding_grid:isWalkableAt(X - 1, Y, Grid3) andalso 
									not path_finding_grid:isWalkableAt(X - 1, Y - Dy, Grid3)) orelse
									(path_finding_grid:isWalkableAt(X + 1, Y, Grid3) andalso 
									not path_finding_grid:isWalkableAt(X + 1, Y - Dy, Grid3)) of
								true ->
									{{X, Y}, Grid3};
								false ->
									case jump(X + 1, Y, X, Y, EndNode, Finder, Grid3) of
										undefined ->
											case jump(X - 1, Y, X, Y, EndNode, Finder, Grid3) of
												undefined ->
													undefined;
												{_, Grid4} ->
													{{X, Y} ,Grid4}
											end;
										{_, Grid4} ->
											{{X, Y}, Grid4}
									end
							end;
						true ->
							undefined
					end,
					case Result of
						undefined ->
							jump(X + Dx, Y + Dy, X, Y, EndNode, Finder, Grid3);
						{_, _} ->
							Result
					end
			end;
		false ->
			undefined
	end.