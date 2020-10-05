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
rangeDifference(range(X1,X2),range(Y1,Y2), D) :- X1>Y1, D is (X1-Y1), !.
rangeDifference(range(X1,X2),range(Y1,Y2), D) :- X2<Y2, D is (Y2-X2), !.
rangeDifference(range(X1,X2),range(Y1,Y2), D) :- D is 0.

% contains(+Range, +Range) --> returns whether a range contains another one
contains(range(X1,X2),range(Y1,Y2)) :- X1=<Y1, X2>=Y2.
bigger(range(X1,X2),range(Y1,Y2)) :- X1>=Y1, X2>=Y2. % range 1 is bigger (comes after) range 2
% maxRange(+Range, +Range, -Range) --> returns merge of two range
maxRange(range(X1,X2), range(Y1,Y2), range(Z1,Z2)) :- min(X1,Y1,Z1), max(X2,Y2,Z2).

% Node: Rectangle with two dimensions.
node(RangeX,RangeY).
% createNode(+Range, +Range, -Node) --> creation of a node, given two range
createNode(R1,R2,node(R1,R2)).
% nodeContains(+Node, +Range, +Range) --> returns whether a node contains a certain interval
nodeContains(node(RangeX1, RangeY1),RangeX2,RangeY2) :- contains(RangeX1,RangeX2), contains(RangeY1, RangeY2).
% minDistantNode(+Range, +Range, +Node, +Node, -Node) --> returns node closer to certain range
leftMinDistantNode(RangeXI,RangeYI, node(RangeX1,RangeY1), node(RangeX2,RangeY2)) :- 
				rangeDifference(RangeXI,RangeX1,L1), rangeDifference(RangeYI,RangeY1,L2),
				rangeDifference(RangeXI,RangeX2,R1), rangeDifference(RangeYI,RangeY2,R2),
				(L1+L2) < (R1+R2), !.

% Tree: left branch, root value, right branch
tree(L,V,R).
% createTree(+Tree, +Node, +Tree, -Tree) --> create a tree given left and right branch and root node
createTree(L,V,R,tree(L,V,R)).
% takeRoot(+Tree, -RootValue) --> returns root value of a tree
takeRoot(tree(L,V,R),V).
% isLeaf(+Node, +Tree) --> returns whether a node is leaf
isLeaf(N,tree(nil,N,nil)).


% Base case: insert a value in an empty tree
insert(N, nil, tree(nil, N, nil)).

% caso appena più avanzato: aggiunta valore ad albero con un valore radice
insert(node(RangeX1, RangeY1), tree(nil,node(RangeX2, RangeY2),nil), O) :- 
				nodeContains(node(RangeX2,RangeY2), RangeX1,RangeY1),
				createTree(tree(nil,node(RangeX1, RangeY1),nil), node(RangeX2, RangeY2), nil, O), !.

insert(node(RangeX1, RangeY1), tree(nil,node(RangeX2, RangeY2),nil), O) :-
				createNode(R1,R2,V), maxRange(RangeX1,RangeX2,R1), maxRange(RangeY1,RangeY2,R2),
				createTree(tree(nil,node(RangeX1, RangeY1),nil),V, tree(nil,node(RangeX2, RangeY2),nil), O), !.

% caso appena più avanzato: aggiunta valore ad albero con un valore radice e un ramo vuoto
insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2), nil), O) :-
				nodeContains(node(RangeX2,RangeY2),RangeX1,RangeY1),
				createTree(L,node(RangeX2, RangeY2), tree(nil, node(RangeX1, RangeY1), nil) ,O), !.
				
insert(node(RangeX1, RangeY1), tree(nil,node(RangeX2, RangeY2),R), O) :-
				nodeContains(node(RangeX2,RangeY2),RangeX1,RangeY1),
				createTree(tree(nil, node(RangeX1, RangeY1),nil), node(RangeX2, RangeY2), R ,O), !.

insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2), nil), O) :-
				createNode(R1,R2,V), maxRange(RangeX1,RangeX2,R1), maxRange(RangeY1,RangeY2,R2),
				createTree(L,V, tree(nil, node(RangeX1, RangeY1), nil) ,O), !.

insert(node(RangeX1, RangeY1), tree(nil,node(RangeX2, RangeY2), R), O) :-
				createNode(R1,R2,V), maxRange(RangeX1,RangeX2,R1), maxRange(RangeY1,RangeY2,R2),
				createTree(tree(nil, node(RangeX1, RangeY1), nil), V, R, O), !.

%%%%%%%%%%%%%%%% da gestire range che si intersecano!!!!

% caso generale: aggiunta valore ad albero 
insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), tree(L2,node(RangeX2, RangeY2),R)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1), %radice contiene valore: vado giù a scegliere uno dei due nodi
				takeRoot(L,V), nodeContains(V,RangeX1,RangeY1),
				insert(node(RangeX1, RangeY1),L,L2), !.
				
insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), tree(L,node(RangeX2, RangeY2),R2)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1), %radice contiene valore: vado giù a scegliere uno dei due nodi
				takeRoot(R,V), nodeContains(V,RangeX1,RangeY1),
				insert(node(RangeX1, RangeY1),R,R2), !.

% caso default
insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), tree(L2,node(RangeX2, RangeY2),R)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1), %radice contiene valore: vado giù a scegliere uno dei due nodi
				takeRoot(L,LV), takeRoot(R,RV),
				leftMinDistantNode(RangeX1,RangeY1,LV,RV),
				insert(node(RangeX1, RangeY1),L,L2), !.

insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), tree(L,node(RangeX2, RangeY2),R2)) :- 
				contains(RangeX2, RangeX1), contains(RangeY2, RangeY1), %radice contiene valore: vado giù a scegliere uno dei due nodi
				insert(node(RangeX1, RangeY1),R,R2), !.

insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), X) :- 
				createNode(R1,R2,V), maxRange(RangeX1,RangeX2,R1), maxRange(RangeY1,RangeY2,R2),
				insert(node(RangeX1, RangeY1), tree(L, V, R), X).


%insert(node(range(6,7), range(6,7)),tree(tree(tree(nil,node(range(3,4),range(3,4)),nil),node(range(2,4),range(2,4)),tree(nil,node(range(2,3),range(2,3)),nil)),node(range(1,6),range(1,6)),tree(nil,node(range(5,6),range(5,6)),nil)), X).
				