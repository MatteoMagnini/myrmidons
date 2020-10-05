% min(+X1, +X2, -R) --> returns min between two numbers
min(X1,X2,R) :- X1<X2, R is X1, !.
min(X1,X2,R) :- R is X2.

% max(+X1, +X2, -R) --> returns max between two numbers
max(X1,X2,R) :- X1>X2, R is X1, !.
max(X1,X2,R) :- R is X2.

% Range and utility function
range(X1,X2).
% rangeDifference(+Range, +Range, -Difference) --> difference between ranges
rangeDifference(range(X1,X2),range(Y1,Y2), D) :- D is (abs(X1-Y1) + abs(X2-Y2)).
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
minDistantNode(RangeXI,RangeYI, node(RangeX1,RangeY1), node(RangeX2,RangeY2), N) :- 
				rangeDifference(RangeXI,RangeX1,L1), rangeDifference(RangeYI,RangeY1,L2),
				rangeDifference(RangeXI,RangeX2,R1), rangeDifference(RangeYI,RangeY2,R2),
				(L1+L2) < (R1+R2), N = node(RangeX1,RangeY1), !.
minDistantNode(RangeXI,RangeYI, node(RangeX1,RangeY1), node(RangeX2,RangeY2), N) :- N = node(RangeX2,RangeY2).

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
				nodeContains(node(RangeX2,RangeY2),RangeX1,RangeY1),
				createTree(tree(nil,node(RangeX1, RangeY1),nil), node(RangeX2, RangeY2), nil, O), !.

insert(node(RangeX1, RangeY1), tree(nil,node(RangeX2, RangeY2),nil), O) :-
				createNode(R1,R2,V), maxRange(RangeX1,RangeX2,R1), maxRange(RangeY1,RangeY2,R2),
				createTree(tree(nil,node(RangeX1, RangeY1),nil),V, nil, O), !.

insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2), nil), O) :-
				nodeContains(node(RangeX2,RangeY2),RangeX1,RangeY1),
				createTree(L,node(RangeX2, RangeY2), tree(nil, node(RangeX1, RangeY1), nil) ,O), !.
				
insert(node(RangeX1, RangeY1), tree(nil,node(RangeX2, RangeY2),R), O) :-
				nodeContains(node(RangeX2,RangeY2),RangeX1,RangeY1),
				createTree(tree(nil, node(RangeX1, RangeY1),nil), node(RangeX2, RangeY2), R ,O), !.

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

insert(node(RangeX1, RangeY1), tree(L,node(RangeX2, RangeY2),R), O) :-
				takeRoot(L,LV), takeRoot(R,RV),
				minDistantNode(RangeX1,RangeY1,LV,RV,N),
				insert(node(RangeX1, RangeY1),tree(nil,N,nil), O).
				

%insert(node(range(1,2), range(1,2)), tree(tree(nil,node(range(0,3),range(0,3)),nil),node(range(0,6),range(0,6)),tree(nil,node(range(3,6),range(3,6)),nil)), X).
%insert(node(range(1,2), range(1,2)), tree(nil,node(range(3,4),range(3,4)),nil), X).
%insertSimpleValue(V, tree(L,V2,R), tree(L,V2,R2)) :- insertSimpleValue(V,R,R2).


%insertSimpleValue( node(range(1,2), range(2,3)), tree(nil,node(range(0,3),range(1,4)),nil), X).