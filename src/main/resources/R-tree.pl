%------------- UTILITY FUNCTIONS---------------
% min(+X1, +X2, -R) --> returns min between two numbers
min(X1,X2,R) :- X1<X2, R is X1, !.
min(X1,X2,R) :- R is X2.

% max(+X1, +X2, -R) --> returns max between two numbers
max(X1,X2,R) :- X1>X2, R is X1, !.
max(X1,X2,R) :- R is X2.

%Range
range(X1,X2).

% rangeDifference(+Range, +Range, -Difference) --> difference between ranges
rangeDifference(range(X1,X2),range(Y1,Y2), D) :- X1>Y1, X2<Y2, D is (X1-Y1) + (Y2-X2), !.
rangeDifference(range(X1,_),range(Y1,_), D) :- X1>Y1, D is (X1-Y1), !.
rangeDifference(range(_,X2),range(_,Y2), D) :- X2<Y2, D is (Y2-X2), !.
rangeDifference(range(_,_),range(_,_), D) :- D is 0.

% contains(+Range, +Range) --> returns whether a range contains another one
contains(range(X1,X2),range(Y1,Y2)) :- X1=<Y1, X2>=Y2.

% maxRange(+Range, +Range, -Range) --> returns merge of two range
mergeRange(range(X1,X2), range(Y1,Y2), range(Z1,Z2)) :- min(X1,Y1,Z1), max(X2,Y2,Z2).

% intersectionRange(+Range, +Range, -Range) --> returns intersection range of two ranges
intersectionRange(range(X1,X2), range(Y1,Y2), range(Z1,Z2)) :- max(X1,Y1,Z1), min(X2,Y2,Z2).

% int
intersects(R1,R2) :- intersectionRange(R1,R2,range(XR,YR)), XR<YR.

% Node: defined by an id and two ranges: X and Y
node(Id,RangeX,RangeY).

% nodeContains(+Node, +Range, +Range) --> returns whether a node contains a certain interval
nodeContains(node(_,RangeX1, RangeY1),RangeX2,RangeY2) :- contains(RangeX1,RangeX2), contains(RangeY1, RangeY2).

% nodeIntersects(+Node, +Range, +Range) --> returns whether a node intersects a certain interval
nodeIntersects(node(_,RangeX1, RangeY1),RangeX2,RangeY2) :- intersects(RangeX1,RangeX2), intersects(RangeY1, RangeY2).

% minDistantNode(+Range, +Range, +Node, +Node, -Node) --> returns whether left node is closer to certain range wrt right node
leftMinDistantNode(RangeXI,RangeYI, node(_,RangeX1,RangeY1), node(_,RangeX2,RangeY2)) :- 
				rangeDifference(RangeXI,RangeX1,L1), rangeDifference(RangeYI,RangeY1,L2),
				rangeDifference(RangeXI,RangeX2,R1), rangeDifference(RangeYI,RangeY2,R2),
				(L1+L2) =< (R1+R2), !.

% nodeMinRange(+Node, +Node, -Node) --> returns a node as result of merge of two nodes
nodeMinRange(nil, N, N).
nodeMinRange(N, nil, N).
nodeMinRange(node(_,RangeX1, RangeY1), node(_,RangeX2, RangeY2), node(none,RangeX3, RangeY3)) :- mergeRange(RangeX1, RangeX2, RangeX3), mergeRange(RangeY1, RangeY2, RangeY3). 

% Tree: left branch, root value, right branch
tree(L,V,R).

% createTree(+Tree, +Node, +Tree, -Tree) --> create a tree given left and right branch and root node
createTree(L,V,R,tree(L,V,R)).

% takeRoot(+Tree, -RootValue) --> returns root value of a tree
takeRoot(nil, nil).
takeRoot(tree(_,V,_),V).

% isLeaf(+Node, +Tree) --> returns whether a node is leaf
isLeaf(N,tree(nil,N,nil)).

%------------- NODE INSERTION ---------------
%insert(+Node, +Tree, -Tree) --> returns tree with added node
% Base case: insert a value in an empty tree
insert(N, nil, tree(nil, N, nil)).

% Add value to a single-value tree -- case: new value is inside root range
insert(node(ID1,RangeX1, RangeY1), tree(nil,node(ID2,RangeX2, RangeY2),nil), O) :- 
				nodeContains(node(ID2,RangeX2,RangeY2), RangeX1,RangeY1),
				createTree(tree(nil,node(ID1,RangeX1, RangeY1),nil), node(ID2,RangeX2, RangeY2), nil, O), !.

% Add value to a single-value tree -- case: new value isn't inside root range -> need to expand it
insert(node(ID1,RangeX1, RangeY1), tree(nil,node(ID2,RangeX2, RangeY2),nil), O) :-
				nodeMinRange(node(ID1,RangeX1, RangeY1), node(ID2,RangeX2, RangeY2), V),
				createTree(tree(nil,node(ID1,RangeX1, RangeY1),nil),V, tree(nil,node(ID2,RangeX2, RangeY2),nil), O), !.

% General case: add value to an arbitrary tree
% If new range does not exceed root range
insert(node(ID1,RangeX1, RangeY1), tree(L,node(ID2,RangeX2, RangeY2),R), tree(L2,node(ID2,RangeX2, RangeY2),R)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1), %root contains value: go down to choose where to go (left)
				takeRoot(L,V), nodeContains(V,RangeX1,RangeY1),
				insert(node(ID1,RangeX1, RangeY1),L,L2), !.
				
insert(node(ID1,RangeX1, RangeY1), tree(L,node(ID2,RangeX2, RangeY2),R), tree(L,node(ID2,RangeX2, RangeY2),R2)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1), %root contains value: go down to choose where to go (right)
				takeRoot(R,V), nodeContains(V,RangeX1,RangeY1),
				insert(node(ID1,RangeX1, RangeY1),R,R2), !.

insert(node(ID1,RangeX1, RangeY1), tree(L,node(ID2,RangeX2, RangeY2),R), tree(L2,node(ID2,RangeX2, RangeY2),R)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1),
				takeRoot(L,LV), takeRoot(R,RV),
 % both left and right branch don't contain new value: calculate min expansion to add new node
				% if there is an empty branch, distance from it will be 0 -> new node will be put there
				leftMinDistantNode(RangeX1,RangeY1,LV,RV),
				insert(node(ID1,RangeX1, RangeY1),L,L2), !.

% Other case: put new value in right branch
insert(node(ID1,RangeX1, RangeY1), tree(L,node(ID2,RangeX2, RangeY2),R), tree(L,node(ID2,RangeX2, RangeY2),R2)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1),
				insert(node(ID1,RangeX1, RangeY1),R,R2), !.
				

% --- rare cases ---
% New node contains root of tree: it becomes root
insert(node(ID1,RangeX1, RangeY1), tree(L,node(ID2,RangeX2, RangeY2),R), O) :- 
				contains(RangeX1, RangeX2), contains(RangeY1, RangeY2),
				createTree(tree(L,node(ID2,RangeX2, RangeY2),R),node(ID1,RangeX1, RangeY1), nil, O), !.
				
% If new range exceeds root range in X or Y, not both
insert(node(ID1,RangeX1, RangeY1), tree(L,node(ID2,RangeX2, RangeY2),R), O) :- 
				nodeMinRange(node(ID1,RangeX1, RangeY1), node(ID2,RangeX2, RangeY2), V),
				insert(node(ID1,RangeX1, RangeY1), tree(L, V, R), O).


%------------- NODE REMOVAL ---------------
% remove(+Node, +Tree, -Tree) --> returns new tree with removed node (without fixing tree) NB: node to be removed has to be leaf
% Base case: remove node from a single value tree
remove(node(ID,RangeX,RangeY), nil, nil).
remove(node(ID,RangeX,RangeY), tree(nil,node(ID,RangeX, RangeY),nil), nil) :- !.

% Iterate to find node to be removed
remove(node(IDI,RangeXI,RangeYI), tree(L,node(ID,RangeX, RangeY),R), tree(T,node(ID,RangeX, RangeY), R)) :- 
				contains(RangeX, RangeXI), contains(RangeY,RangeYI),
				takeRoot(L,V), nodeContains(V,RangeXI,RangeYI),
				remove(node(IDI,RangeXI,RangeYI),L, T), !.
								
remove(node(IDI,RangeXI,RangeYI), tree(L,node(ID,RangeX, RangeY),R), tree(L,node(ID,RangeX, RangeY), T)) :- 
				contains(RangeX, RangeXI), contains(RangeY,RangeYI),
				takeRoot(R,V), nodeContains(V,RangeXI,RangeYI),
				remove(node(IDI,RangeXI,RangeYI),R, T), !.

% Find node and remove it
remove(node(IDI,RangeXI,RangeYI), tree(L,node(ID,RangeX, RangeY),R),tree(L,node(ID,RangeX, RangeY),R)).

% fixTree(+Tree, -Tree) --> returns fixed tree (with minimal intervals wrt nodes)
% Base case: empty tree
fixTree(nil, nil).

% Single value tree: value remains the same
fixTree(tree(nil,node(ID1,RangeX1,RangeY1), nil), tree(nil, node(ID1,RangeX1, RangeY1), nil)):- !.

% Tree with an empty branch: substitute node with leaf (minimal interval)
fixTree(tree(nil, node(_,RangeX1, RangeY1), R), Tree) :- 
				takeRoot(R, RV), isLeaf(RV, R),
				createTree(nil, RV, nil, Tree), !.		
				
fixTree(tree(L, node(_,RangeX1, RangeY1), nil), Tree) :- 
				takeRoot(L, LV), isLeaf(LV,L),
				createTree(nil, LV, nil, Tree), !.
				
fixTree(tree(nil, node(_,RangeX1, RangeY1), R), Tree) :- 
				takeRoot(R, RV), fixTree(R, Tree), !.
			
fixTree(tree(L, node(_,RangeX1, RangeY1), nil), Tree) :- 
				takeRoot(L, LV), fixTree(L, Tree), !.

% General case: every node has to be merge of its childern intervals -> inverse recursion
fixTree(tree(L, node(_,RangeX1, RangeY1), R), Tree) :- 
				fixTree(L, T1), fixTree(R, T2), 
				takeRoot(T1, LV), takeRoot(T2, RV), nodeMinRange(LV, RV, V),
				createTree(T1, V, T2, Tree).

% removeWithFix(+Node, +Tree, -Tree) --> returns new tree with removed node (fixing tree) NB: node to be removed has to be leaf
removeWithFix(Node, ITree, OTree) :- remove(Node, ITree, TTree), fixTree(TTree, OTree).


% getLeavesList(+Tree, -List) --> returns a list containing leaves of a tree
getLeavesList(nil, nil).
getLeavesList(tree(nil, V, nil), [V]) :- !.
getLeavesList(tree(L, V, R), List) :- getLeavesList(L, L1), getLeavesList(R, L2), append(L1,L2,List).


%------------- QUERY ---------------
% query(+Tree, +RangeX, +RangeY, -Tree) --> returns list of nodes that respond to query
query(nil, RangeX1, RangeY1, nil).

query(tree(L,V,R), RangeXI,RangeYI, [V]) :- 
				nodeIntersects(V,RangeXI,RangeYI),
				isLeaf(V, tree(L,V,R)), !.

query(tree(L,V,R), RangeXI,RangeYI, List) :- 
				nodeIntersects(V,RangeXI,RangeYI),
				query(L,RangeXI,RangeYI,L1),
				query(R,RangeXI,RangeYI,L2),
				append(L1, L2, List), !.

query(tree(L,V,R), RangeX1, RangeY1, []).
