%------------- UTILITY FUNCTIONS---------------
% min(+X1, +X2, -R) --> returns min between X1 and X2
min(X1,X2,X1) :- X1<X2, !.
min(X1,X2,X2).

% max(+X1, +X2, -R) --> returns max between X1 and X2
max(X1,X2,X1) :- X1>X2, !.
max(X1,X2,X2).

% range(X1,X2) --> X1: lower bound, X2: uppen bound

% rangeDifference(+RangeX, +RangeY, -Difference) --> returns how much RangeY need to be expanded to include RangeX
rangeDifference(range(X1,X2),range(Y1,Y2), D) :- X1>Y1, X2<Y2, D is (X1-Y1) + (Y2-X2), !.
rangeDifference(range(X1,_),range(Y1,_), D) :- X1>Y1, D is (X1-Y1), !.
rangeDifference(range(_,X2),range(_,Y2), D) :- X2<Y2, D is (Y2-X2), !.
rangeDifference(range(_,_),range(_,_), D) :- D is 0.

% contains(+RangeX, +RangeY) --> returns whether RangeX contains RangeY
contains(range(X1,X2),range(Y1,Y2)) :- X1=<Y1, X2>=Y2.

% maxRange(+RangeX, +RangeY, -RangeZ) --> returns merge of RangeX and RangeY
mergeRange(range(X1,X2),range(Y1,Y2), range(Z1,Z2)) :- min(X1,Y1,Z1), max(X2,Y2,Z2).

% intersectionRange(+RangeX, +RangeY, -RangeZ) --> returns intersection range of RangeX and RangeY
intersectionRange(range(X1,X2), range(Y1,Y2), range(Z1,Z2)) :- max(X1,Y1,Z1), min(X2,Y2,Z2).

% intersects(+Range1, +Range2) --> returns whether Range1 and Range2 are intersected
intersects(R1,R2) :- intersectionRange(R1,R2,range(XR,YR)), XR<YR.

% node(Id,RangeX,RangeY) --> Id: node id, RangeX: range along x coordinate, RangeY; range along y coordinate

% nodeContains(+Node, +RangeX, +RangeY) --> returns whether a node contains a certain interval
nodeContains(node(_,RangeX1, RangeY1),RangeX2,RangeY2) :- contains(RangeX1,RangeX2), contains(RangeY1,RangeY2).

% nodeIntersects(+Node, +RangeX, +RangeY) --> returns whether a node intersects a certain interval
nodeIntersects(node(_,RangeX1, RangeY1),RangeX2,RangeY2) :- intersects(RangeX1,RangeX2), intersects(RangeY1, RangeY2).

% leftMinDistantNode(+RangeX, +RangeY, +NodeL, +NodeR) --> returns whether left node is closer to certain range wrt right node
leftMinDistantNode(RangeXI,RangeYI, node(_,RangeXL,RangeYL), node(_,RangeXR,RangeYR)) :-
				rangeDifference(RangeXI,RangeXL,L1), rangeDifference(RangeYI,RangeYL,L2),
				rangeDifference(RangeXI,RangeXR,R1), rangeDifference(RangeYI,RangeYR,R2),
				(L1+L2) =< (R1+R2).

% nodeMinRange(+Node1, +Node2, -Node3) --> returns a node as result of merge of two nodes (with none as id)
nodeMinRange(nil, N, N).
nodeMinRange(N, nil, N).
nodeMinRange(node(_,RangeX1, RangeY1), node(_,RangeX2, RangeY2), node(none,RangeX3, RangeY3)) :-
				mergeRange(RangeX1, RangeX2, RangeX3), mergeRange(RangeY1, RangeY2, RangeY3).

% tree(L,V,R): left branch, root value, right branch

% createTree(+TreeL, +Node, +TreeR, -Tree) --> create a tree given left and right branch and root node
createTree(L,V,R,tree(L,V,R)).

% takeRoot(+Tree, -RootValue) --> returns root value of a tree
takeRoot(nil, nil).
takeRoot(tree(_,V,_),V).

% isLeaf(+Node, +Tree) --> returns whether a node is leaf
isLeaf(N,tree(nil,N,nil)).

%------------- NODE INSERTION ---------------
%insert(+Node, +TreeI, -TreeO) --> returns tree with added node
% Base case: insert a value in an empty tree
insert(N, nil, tree(nil, N, nil)).

% Add value to a single-value tree -- case: new value is inside root range - new node put on left side by default
insert(node(ID1,RangeX1,RangeY1), tree(nil,N2,nil), O) :-
				nodeContains(N2,RangeX1,RangeY1),
				createTree(tree(nil,node(ID1,RangeX1, RangeY1),nil), N2, nil, O), !.

% Add value to a single-value tree -- case: new value isn't inside root range -> need to expand it
insert(N1, tree(nil,N2,nil), O) :-
				nodeMinRange(N1, N2, V),
				createTree(tree(nil,N1,nil),V, tree(nil,N2,nil), O), !.

% General case: add value to an arbitrary tree
% If new range does not exceed root range
insert(node(ID1,RangeX1, RangeY1), tree(L,N2,R), tree(L2,N2,R)) :-
				nodeContains(N2,RangeX1, RangeY1), %root contains value: go down to choose where to go (left)
				takeRoot(L,V), nodeContains(V,RangeX1,RangeY1),
				insert(node(ID1,RangeX1, RangeY1),L,L2), !.

insert(node(ID1,RangeX1, RangeY1), tree(L,N2,R), tree(L,N2,R2)) :-
				nodeContains(N2,RangeX1,RangeY1), %root contains value: go down to choose where to go (right)
				takeRoot(R,V), nodeContains(V,RangeX1,RangeY1),
				insert(node(ID1,RangeX1, RangeY1),R,R2), !.

insert(node(ID1,RangeX1, RangeY1), tree(L,N2,R), tree(L2,N2,R)) :-
				nodeContains(N2,RangeX1,RangeY1),
				takeRoot(L,LV), takeRoot(R,RV),
                % both left and right branch don't contain new value: calculate min expansion to add new node
				% if there is an empty branch, distance from it will be 0 -> new node will be put there
				leftMinDistantNode(RangeX1,RangeY1,LV,RV),
				insert(node(ID1,RangeX1, RangeY1),L,L2), !.

% Other case: put new value in right branch
insert(node(ID1,RangeX1, RangeY1), tree(L,N2,R), tree(L,N2,R2)) :-
				nodeContains(N2, RangeX1,RangeY1),
				insert(node(ID1,RangeX1, RangeY1),R,R2), !.


% --- Case is not feasible in out application ---
% New node contains root of tree: it becomes root
insert(node(ID1,RangeX1, RangeY1), tree(L,node(ID2,RangeX2, RangeY2),R), O) :-
				contains(RangeX1, RangeX2), contains(RangeY1, RangeY2),
				createTree(tree(L,node(ID2,RangeX2, RangeY2),R),node(ID1,RangeX1, RangeY1), nil, O), !.

% If new range exceeds root range in X or Y, not both
insert(node(ID1,RangeX1, RangeY1), tree(L,node(ID2,RangeX2, RangeY2),R), O) :-
				nodeMinRange(node(ID1,RangeX1, RangeY1), node(ID2,RangeX2, RangeY2), V),
				insert(node(ID1,RangeX1, RangeY1), tree(L, V, R), O).


%------------- NODE REMOVAL ---------------
% remove(+Node, +TreeI, -TreeO) --> returns new tree with removed node (without fixing tree) NB: node to be removed has to be leaf
remove(N, nil, nil).
% Base case: remove node from a single value tree
remove(N, tree(nil,N,nil), nil) :- !.

% Iterate to find node to be removed
remove(node(IDI,RangeXI,RangeYI), tree(L,N2,R), tree(T,N2,R)) :-
				nodeContains(N2,RangeXI,RangeYI),
				takeRoot(L,V), nodeContains(V,RangeXI,RangeYI),
				remove(node(IDI,RangeXI,RangeYI),L, T), !.

remove(node(IDI,RangeXI,RangeYI), tree(L,N2,R), tree(L,N2,T)) :-
				nodeContains(N2,RangeXI,RangeYI),
				takeRoot(R,V), nodeContains(V,RangeXI,RangeYI),
				remove(node(IDI,RangeXI,RangeYI),R, T), !.

% Element to be removed isn't in tree
remove(_, T, T).

% fixTree(+TreeI, -TreeO) --> returns fixed tree (with minimal intervals wrt nodes)
% Base case: empty tree
fixTree(nil, nil).

% Single value tree: value remains the same
fixTree(tree(nil,N,nil), tree(nil,N,nil)):- !.

% Tree with an empty branch: substitute node with leaf (minimal interval)
fixTree(tree(nil,_, R), Tree) :-
				takeRoot(R, RV), isLeaf(RV, R),
				createTree(nil, RV, nil, Tree), !.

fixTree(tree(L, _, nil), Tree) :-
				takeRoot(L, LV), isLeaf(LV,L),
				createTree(nil, LV, nil, Tree), !.

fixTree(tree(nil, _, R), Tree) :-
				takeRoot(R, RV), fixTree(R, Tree), !.

fixTree(tree(L, _, nil), Tree) :-
				takeRoot(L, LV), fixTree(L, Tree), !.

% General case: every node has to be merge of its childern intervals -> inverse recursion
fixTree(tree(L, _, R), Tree) :-
				fixTree(L, T1), fixTree(R, T2),
				takeRoot(T1, LV), takeRoot(T2, RV), nodeMinRange(LV, RV, V),
				createTree(T1, V, T2, Tree).

% removeWithFix(+Node, +TreeI, -TreeO) --> returns new tree with removed node (fixing tree) NB: node to be removed has to be leaf
removeWithFix(Node, ITree, OTree) :- remove(Node, ITree, TTree), fixTree(TTree, OTree).


% getLeavesList(+Tree, -List) --> returns a list containing leaves of a tree
getLeavesList(nil, []).
getLeavesList(tree(nil, V, nil), [V]) :- !.
getLeavesList(tree(L, V, R), List) :- getLeavesList(L, L1), getLeavesList(R, L2), append(L1,L2,List).

%------------- QUERY ---------------
% query(+Tree, +RangeX, +RangeY, -Tree) --> returns list of nodes that respond to query
query(nil, RangeX1, RangeY1, []).

query(tree(L,V,R), RangeXI,RangeYI, [V]) :-
				nodeIntersects(V,RangeXI,RangeYI),
				isLeaf(V, tree(L,V,R)), !.

query(tree(L,V,R), RangeXI,RangeYI, List) :-
				nodeIntersects(V,RangeXI,RangeYI),
				query(L,RangeXI,RangeYI,L1),
				query(R,RangeXI,RangeYI,L2),
				append(L1, L2, List), !.

query(tree(L,V,R), RangeX1, RangeY1, []).
