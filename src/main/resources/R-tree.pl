% min(+X1, +X2, -R) --> returns min between two numbers
min(X1,X2,R) :- X1<X2, R is X1, !.
min(X1,X2,R) :- R is X2.

% max(+X1, +X2, -R) --> returns max between two numbers
max(X1,X2,R) :- X1>X2, R is X1, !.
max(X1,X2,R) :- R is X2.

% Range and utility function
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

% Node: Rectangle with two dimensions.
node(RangeX,RangeY).
% createNode(+Range, +Range, -Node) --> creation of a node, given two range
createNode(R1,R2,node(R1,R2)).
% nodeContains(+Node, +Range, +Range) --> returns whether a node contains a certain interval
nodeContains(node(RangeX1, RangeY1),RangeX2,RangeY2) :- contains(RangeX1,RangeX2), contains(RangeY1, RangeY2).
% minDistantNode(+Range, +Range, +Node, +Node, -Node) --> returns whether left node is closer to certain range wrt right node
leftMinDistantNode(RangeXI,RangeYI, node(RangeX1,RangeY1), node(RangeX2,RangeY2)) :- 
				rangeDifference(RangeXI,RangeX1,L1), rangeDifference(RangeYI,RangeY1,L2),
				rangeDifference(RangeXI,RangeX2,R1), rangeDifference(RangeYI,RangeY2,R2),
				(L1+L2) =< (R1+R2), !.
nodeMinRange(node(RangeX1, RangeY1), node(RangeX2, RangeY2), node(RangeX3, RangeY3)) :- mergeRange(RangeX1, RangeX2, RangeX3), mergeRange(RangeY1, RangeY2, RangeY3). 

% Tree: left branch, root value, right branch
tree(L,V,R).
% createTree(+Tree, +Node, +Tree, -Tree) --> create a tree given left and right branch and root node
createTree(L,V,R,tree(L,V,R)).
% takeRoot(+Tree, -RootValue) --> returns root value of a tree
takeRoot(tree(_,V,_),V).
% isLeaf(+Node, +Tree) --> returns whether a node is leaf
isLeaf(N,tree(nil,N,nil)).

% Base case: insert a value in an empty tree
insert(N, nil, tree(nil, N, nil)).

% Add value to a single-value tree -- case: new value is inside root range
insert(node(RangeX1, RangeY1), tree(nil,node(RangeX2, RangeY2),nil), O) :- 
				nodeContains(node(RangeX2,RangeY2), RangeX1,RangeY1),
				createTree(tree(nil,node(RangeX1, RangeY1),nil), node(RangeX2, RangeY2), nil, O), !.

% Add value to a single-value tree -- case: new value isn't inside root range -> need to expand it
insert(node(RangeX1, RangeY1), tree(nil,node(RangeX2, RangeY2),nil), O) :-
				createNode(R1,R2,V), mergeRange(RangeX1,RangeX2,R1), mergeRange(RangeY1,RangeY2,R2),
				createTree(tree(nil,node(RangeX1, RangeY1),nil),V, tree(nil,node(RangeX2, RangeY2),nil), O), !.

% General case: add value to an arbitrary tree
% If new range does not exceed root range
insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), tree(L2,node(RangeX2, RangeY2),R)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1), %radice contiene valore: vado giù a scegliere uno dei due nodi
				takeRoot(L,V), nodeContains(V,RangeX1,RangeY1),
				insert(node(RangeX1, RangeY1),L,L2), !.
				
insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), tree(L,node(RangeX2, RangeY2),R2)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1), %radice contiene valore: vado giù a scegliere uno dei due nodi
				takeRoot(R,V), nodeContains(V,RangeX1,RangeY1),
				insert(node(RangeX1, RangeY1),R,R2), !.


insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), tree(L2,node(RangeX2, RangeY2),R)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1), %radice contiene valore: vado giù a scegliere uno dei due nodi
				takeRoot(L,LV), takeRoot(R,RV),
				leftMinDistantNode(RangeX1,RangeY1,LV,RV), %cosa carina: se ho un branch vuoto, la distanza sarà nil per caso default della distanza e valore nuovo verrà messo li!
				insert(node(RangeX1, RangeY1),L,L2), !.

insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), tree(L,node(RangeX2, RangeY2),R2)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1), %radice contiene valore: vado giù a scegliere uno dei due nodi
				insert(node(RangeX1, RangeY1),R,R2), !.

% If new range contains root
insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), O) :- 
				contains(RangeX1, RangeX2), contains(RangeY1, RangeY2),
				createTree(tree(L,node(RangeX2, RangeY2),R),node(RangeX1, RangeY1), nil, O), !.
				
% If new range exceeds root range in X or Y, not both
insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), O) :- 
				createNode(R1,R2,V), mergeRange(RangeX1,RangeX2,R1), mergeRange(RangeY1,RangeY2,R2),
				insert(node(RangeX1, RangeY1), tree(L, V, R), O).

%insert(node(range(6,7), range(6,7)),tree(tree(tree(nil,node(range(3,4),range(3,4)),nil),node(range(2,4),range(2,4)),tree(nil,node(range(2,3),range(2,3)),nil)),node(range(1,6),range(1,6)),tree(nil,node(range(5,6),range(5,6)),nil)), X).

remove(node(RangeX,RangeY), tree(nil,node(RangeX, RangeY),nil), nil) :- !.

remove(node(RangeXI,RangeYI), tree(L,node(RangeX, RangeY),R), tree(T,node(RangeX, RangeY), R)) :- 
				contains(RangeX, RangeXI), contains(RangeY,RangeYI),
				takeRoot(L,V), nodeContains(V,RangeXI,RangeYI),
				remove(node(RangeXI,RangeYI),L, T), !.
								
remove(node(RangeXI,RangeYI), tree(L,node(RangeX, RangeY),R), tree(L,node(RangeX, RangeY), T)) :- 
				contains(RangeX, RangeXI), contains(RangeY,RangeYI),
				takeRoot(R,V), nodeContains(V,RangeXI,RangeYI),
				remove(node(RangeXI,RangeYI),R, T), !.

remove(node(RangeXI,RangeYI), tree(L,node(RangeX, RangeY),R),tree(L,node(RangeX, RangeY),R)).

fixTree(nil, nil).
fixTree(tree(nil,node(RangeX1,RangeY1), nil), tree(nil, node(RangeX1, RangeY1), nil)):- !.
fixTree(tree(nil, node(RangeX1, RangeY1), R), Tree) :- 
				takeRoot(R, RV),
				fixTree(R, T2), createTree(nil, RV, T2, Tree), !.		
fixTree(tree(L, node(RangeX1, RangeY1), nil), Tree) :- 
				takeRoot(L, LV),
				fixTree(L, T1), createTree(T1, LV, nil, Tree), !.
fixTree(tree(L, node(RangeX1, RangeY1), R), Tree) :- 
				takeRoot(L, LV), takeRoot(R, RV), nodeMinRange(LV, RV, V),
				fixTree(L, T1), fixTree(R, T2), createTree(T1, V, T2, Tree).

removeWithFix(Node, ITree, OTree) :- remove(Node, ITree, TTree), fixTree(TTree, OTree).
			
%remove( node(range(1,2), range(1,2)), tree(tree(nil, node(range(1,2), range(1,2)), nil), node(range(1,3), range(1,3)), tree(nil, node(range(2,3), range(2,3)), nil)), X).				

getLeavesList(tree(nil, V, nil), [V]) :- !.
getLeavesList(tree(L, V, R), List) :- getLeavesList(L, L1), getLeavesList(R, L2), append(L1,L2,List).

%%%%%%%%%%%%%%%%%%%%% QUERY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

query(tree(L,node(RangeX, RangeY),R),RangeXI,RangeYI, OTree) :- 
				contains(RangeX, RangeXI), contains(RangeY,RangeYI),
				takeRoot(L,V), nodeContains(V,RangeXI,RangeYI),
				query(L, RangeXI,RangeYI, OTree), !.

query(tree(L,node(RangeX, RangeY),R),RangeXI,RangeYI, OTree) :- 
				contains(RangeX, RangeXI), contains(RangeY,RangeYI),
				takeRoot(R,V), nodeContains(V,RangeXI,RangeYI),
				query(R, RangeXI,RangeYI, OTree), !.

query(tree(L,node(RangeX, RangeY),R),RangeXI,RangeYI, tree(L,node(RangeX, RangeY),R)) :-
				contains(RangeX, RangeXI), contains(RangeY,RangeYI).

queryToList(Tree, Range1, Range2, List) :- query(Tree, Range1, Range2, Otree), getLeavesList(Otree, List).

%tree(tree(tree(nil,node(range(2,3),range(7,8)),nil),node(range(2,4),range(5,8)),tree(nil,node(range(3,4),range(5,6)),nil)),node(range(1,7),range(1,8)),tree(tree(nil,node(range(6,7),range(3,4)),nil),node(range(1,7),range(1,4)),tree(nil,node(range(1,2),range(1,2)),nil)))