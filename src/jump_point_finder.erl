-module(jump_point_finder).

-include("path_finding.hrl").

-export([finder/6]).

finder(StartX, StartY, EndX, EndY, Finder, Grid) ->
	OpenList = path_finding_heap:heap(),
	StartNode = path_finding_grid:getNodeAt(StartX, StartY, Grid),
	EndNode = path_finding_grid:getNodeAt(EndX, EndY, Grid),

	StartNode2 = StartNode#path_finding_node{g = 0, f = 0, opened = true},
	OpenList2 = path_finding_heap:push(StartNode2#path_finding_node.f, StartNode2, OpenList),
	
	do_finder(OpenList2, EndNode, Finder, Grid).

do_finder([], _, _, _) -> [];
do_finder(OpenList, EndNode, Finder, Grid) ->
	{{_Key, Node = #path_finding_node{x = X, y = Y}}, OpenList2} = path_finding_heap:pop(OpenList),
	Node2 = Node#path_finding_node{closed = true},
	Grid2 = path_finding_grid:updateNodeAt(X, Y, Node2, Grid),
	case X =:= EndNode#path_finding_node.x andalso Y =:= EndNode#path_finding_node.y of
		true ->
			path_finding_util:expandPath(path_finding_util:backtrace(Node2, Grid2));
		false ->
			{OpenList3, Grid3} = identifySuccessors(OpenList2, Node, EndNode, Finder, Grid2),
			do_finder(OpenList3, EndNode, Finder, Grid3)
	end.


identifySuccessors(OpenList, Node = #path_finding_node{x = X, y = Y, g = G}, EndNode = #path_finding_node{x = EndX, y = EndY}, 
	Finder = #path_finding_finder{diagonal_movement = DiagonalMovement, heuristic = Heuristic}, Grid) ->
	M = case DiagonalMovement of
		?DM_IfAtMostOneObstacle ->
			jpf_move_diagonally_if_at_most_one_obstacle;
		?DM_OnlyWhenNoObstacles ->
			jpf_move_diagonally_if_no_obstacle;
		?DM_Always ->
			jpf_always_move_diagonally;
		?DM_Never ->
			jpf_never_move_diagonally;
		_ ->
			jpf_move_diagonally_if_at_most_one_obstacle
	end,
	Neighbors = M:findNeighbors(Node, Grid),

	F = fun({NeighborNodex, NeighborNodeY}, {OpenListAcc, GridAcc}) ->
		case M:jump(NeighborNodex, NeighborNodeY, X, Y, EndNode, Finder, GridAcc) of
			{{JX, JY}, Grid2Acc} ->
				% lager:info("jumpPoint[ ~w ~w ] ~n",[JX, JY]),
				JumpNode = #path_finding_node{
					closed = JumpNodeClosed, 
					opened = JumpNodeOpened, 
					g = JumpNodeG,
					h = JumpNodeH
				} = path_finding_grid:getNodeAt(JX, JY, Grid2Acc),
				case JumpNodeClosed of
					true ->
						{OpenListAcc, Grid2Acc};	
					_ ->
						D = ?Heuristic_Octile(abs(JX - X), abs(JY - Y)),
						Ng = G + D,
						% lager:info("JumpNodeOpened ~w ~w ~w",[JumpNodeOpened, Ng, JumpNodeG]),
						% lager:info("EndX ~w ~w",[EndX, EndY]),
						case not JumpNodeOpened orelse Ng < JumpNodeG of
							true ->
								NewJumpNodeG = Ng,
								NewJumpNodeH = 
									case JumpNodeH of
										0 -> 
											Heuristic(abs(JX - EndX), abs(JY - EndY));
										_ -> 
											JumpNodeH
									end,
								NewJumpNodeF = NewJumpNodeG + NewJumpNodeH,
								NewjumpNode = JumpNode#path_finding_node{g = Ng, h = NewJumpNodeH, f = NewJumpNodeF, parent = Node},
								case not JumpNodeOpened of
									true ->
										NewjumpNode2 = NewjumpNode#path_finding_node{opened = true},
										OpenList2Acc = path_finding_heap:push(NewJumpNodeF, NewjumpNode2, OpenListAcc),
										Grid3Acc = path_finding_grid:updateNodeAt(JX, JY, NewjumpNode2, Grid2Acc),
										{OpenList2Acc, Grid3Acc};
									false ->
										OpenList2Acc = path_finding_heap:update(NewJumpNodeF, NewjumpNode, OpenListAcc),
										Grid3Acc = path_finding_grid:updateNodeAt(JX, JY, NewjumpNode, Grid2Acc),
										{OpenList2Acc, Grid3Acc}
								end;
							false ->
								{OpenListAcc, Grid2Acc}
						end
				end;
			_ ->
				% lager:info("jumpPoint null"),
				{OpenListAcc, GridAcc}
		end
	end,
	{OpenListAcc, GridAcc} = lists:foldl(F, {OpenList, Grid}, Neighbors),
	{OpenListAcc, GridAcc}.
