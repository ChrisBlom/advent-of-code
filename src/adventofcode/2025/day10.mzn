% Number of rows (number of equations)
int: R;
% Number of columns (number of variables in v)
int: C;

% The binary matrix M (R rows, C columns)
array[1..R, 1..C] of {0, 1}: M;

array[1..C] of int: x;

array[1..R] of var 0..max(x): v;

constraint forall(c in 1..C) (
    sum(r in 1..R) (M[r, c] * v[r]) = x[c]
);

solve minimize sum(r in 1..R)( v[r] );


% Output the resulting vector v
output [
    show(v)
];
