-module(jpf_move_diagonally_if_no_obstacle).

-include("path_finding.hrl").

-export([findNeighbors/2, jump/7]).

findNeighbors(Node = #path_finding_node{parent = undefined}, Grid) ->
	NeighborNodes = path_finding_grid:getNeighbors(Node, ?DM_OnlyWhenNoObstacles, Grid),
	% lager:info("NeighborNodes ~w",[NeighborNodes]),
	Neighbors = [{NeighborNodex, NeighborNodeY} || #path_finding_node{x = NeighborNodex, y = NeighborNodeY} <- NeighborNodes],
	Neighbors;
findNeighbors(#path_finding_node{x = X, y = Y, parent = #path_finding_node{x = Px, y = Py}}, Grid) ->
	Neighbors = [],
	Dx = trunc((X - Px) / max(abs(X - Px), 1)),
	Dy = trunc((Y - Py) / max(abs(Y - Py), 1)),
	case Dx =/= 0 andalso Dy =/= 0 of
		true ->
			Neighbors2 = case path_finding_grid:isWalkableAt(X, Y + Dy, Grid) of
					true ->
						[{X, Y + Dy} | Neighbors];
					false ->
						Neighbors
				end,
				Neighbors3 = case path_finding_grid:isWalkableAt(X + Dx, Y, Grid) of
					true ->
						[{X + Dx, Y} | Neighbors2];
					false ->
						Neighbors2
				end,
				Neighbors4 = case path_finding_grid:isWalkableAt(X, Y + Dy, Grid) andalso path_finding_grid:isWalkableAt(X + Dx, Y, Grid) of
					true ->
						[{X + Dx, Y + Dy} | Neighbors3];	
					false ->
						Neighbors3
				end,
				Neighbors4;
		false when Dx =/= 0 ->
			%% search horizontally/vertically
			IsNextWalkable = path_finding_grid:isWalkableAt(X + Dx, Y, Grid),
			IsTopWalkable = path_finding_grid:isWalkableAt(X, Y + 1, Grid),
			IsBottomWalkable = path_finding_grid:isWalkableAt(X, Y - 1, Grid),
			Neighbors5 = case IsNextWalkable of
				true ->
					Neighbors2 = [{X + Dx, Y} | Neighbors],
					Neighbors3 = case IsTopWalkable of
						true ->
							[{X + Dx, Y + 1} | Neighbors2];
						false ->
							Neighbors2
					end,
					Neighbors4 = case IsBottomWalkable of
						true ->
							[{X + Dx, Y - 1} | Neighbors3];
						false -> 
							Neighbors3
					end,
					Neighbors4;
				false ->
					Neighbors 
			end,
			Neighbors6 = case IsTopWalkable of
				true ->
					[{X, Y + 1} | Neighbors5];
				false ->
					Neighbors5
			end,
			Neighbors7 = case IsBottomWalkable of
				true ->
					[{X, Y - 1} | Neighbors6];
				false ->
					Neighbors6
			end,
			Neighbors7;
		false when Dy =/= 0 ->
			IsNextWalkable = path_finding_grid:isWalkableAt(X, Y + Dy, Grid),
			IsRightWalkable = path_finding_grid:isWalkableAt(X + 1, Y, Grid),
			IsLeftWalkable = path_finding_grid:isWalkableAt(X - 1, Y, Grid),
			Neighbors5 = case IsNextWalkable of
				true ->
					Neighbors2 = [{X, Y + Dy} | Neighbors],
					Neighbors3 = case IsRightWalkable of
						true ->
							[{X + 1, Y + Dy} | Neighbors2];
						false ->
							Neighbors2
					end,
					Neighbors4 = case IsLeftWalkable of
						true ->
							[{X - 1, Y + Dy} | Neighbors3];
						false ->
							Neighbors3
					end,
					Neighbors4;
				false ->
					Neighbors
			end,
			Neighbors6 = case IsRightWalkable of
				true ->
					[{X + 1, Y} | Neighbors5];
				false ->
					Neighbors5
			end,
			Neighbors7 = case IsLeftWalkable of
				true ->
					[{X - 1, Y} | Neighbors6];
				false ->
					Neighbors6
			end,
			Neighbors7
	end.

jump(X, Y, Px, Py, EndNode = #path_finding_node{x = EndX, y = EndY}, Finder = #path_finding_finder{track_jump_recursion = TrackJumpRecursion}, Grid) ->
	% lager:info("check jump ~w ~w ~w ~w ~n",[X, Y, Px, Py]),
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
					Result = case Dx =/= 0 andalso Dy =/= 0 of
						true ->
							case jump(X + Dx, Y, X, Y, EndNode, Finder, Grid3) of
								undefined ->
									case jump(X, Y + Dy, X, Y, EndNode, Finder, Grid3) of
										undefined ->
											undefined;
										{_, Grid4} ->
											{{X, Y} ,Grid4}
									end;
								{_, Grid4} ->
									{{X, Y}, Grid4}
							end;
						false when Dx =/= 0 -> %%horizontally, moveing along x
							case (path_finding_grid:isWalkableAt(X, Y - 1, Grid3) andalso 
									not path_finding_grid:isWalkableAt(X - Dx, Y - 1, Grid3)) orelse
									(path_finding_grid:isWalkableAt(X, Y + 1, Grid3) andalso 
									not path_finding_grid:isWalkableAt(X - Dx, Y + 1, Grid3)) of
								true ->
									{{X, Y}, Grid3};
								false ->
									undefined
							end;
						false -> 	%%vertically, moveing along y
							case (path_finding_grid:isWalkableAt(X - 1, Y, Grid3) andalso
									not path_finding_grid:isWalkableAt(X - 1, Y - Dy, Grid3)) orelse
									(path_finding_grid:isWalkableAt(X + 1, Y, Grid3) andalso
									not path_finding_grid:isWalkableAt(X + 1, Y - Dy, Grid3)) of
								true ->
									{{X, Y}, Grid3};
								false ->
									undefined
							end
					end,
					case Result of
						undefined ->
							case path_finding_grid:isWalkableAt(X + Dx, Y, Grid3) andalso path_finding_grid:isWalkableAt(X, Y + Dy, Grid3) of
								true ->
									% lager:info("dump true ~w, ~w~n",[X + Dx, Y + Dy]),
									jump(X + Dx, Y + Dy, X, Y, EndNode, Finder, Grid3);
								false ->
									% lager:info("dump false ~w ~w",[X + Dx, Y + Dy]),
									undefined
							end;
						{_, _} ->
							Result
					end
			end;
		false ->
			undefined
	end.
