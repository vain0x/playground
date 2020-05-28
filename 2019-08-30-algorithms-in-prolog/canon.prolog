% Program canonicalization.

append3(Xs1, Xs2, Xs3, Xs) :-
    append(Xs2, Xs3, Xs4),
    append(Xs1, Xs4, Xs).

canon_expr(int(X), int(X), []).

canon_expr(A, A, []) :-
    atom(A).

canon_expr(call(F, X), Y, Stmts) :-
    canon_expr(F, CanonF, StmtsF),
    canon_expr(X, CanonX, StmtsX),
    append3(StmtsF, StmtsX, [let_stmt(Y, call(CanonF, CanonX))], Stmts).

canon_expr(let_stmt(Pat, Init), int(0), Stmts) :-
    canon_expr(Init, CanonInit, StmtsInit),
    append(StmtsInit, [let_stmt(Pat, CanonInit)], Stmts).

canon_expr([X], CanonX, Stmts) :-
    canon_expr(X, CanonX, Stmts).

canon_expr([X | Xs], CanonXs, Stmts) :-
    canon_stmt(X, StmtsX),
    canon_expr(Xs, CanonXs, StmtsXs),
    append(StmtsX, StmtsXs, Stmts).

canon_stmt([], []) :- !.

canon_stmt([X | Xs], Stmts) :-
    !,
    canon_stmt(X, StmtsX),
    canon_stmt(Xs, StmtsXs),
    append(StmtsX, StmtsXs, Stmts).

canon_stmt(call(F, X), [call(CanonF, CanonX) | Stmts]) :-
       	   !,
    canon_expr(F, CanonF, StmtsF),
    canon_expr(X, CanonX, StmtsX),
    append(StmtsF, StmtsX, Stmts).

canon_stmt(let_stmt(Pat, Init), [let_stmt(Pat, CanonInit) | Stmts]) :-
    !,
    canon_expr(Init, CanonInit, Stmts).

canon_stmt(E, [expr_stmt(CanonE) | Stmts]) :-
    canon_expr(E, CanonE, Stmts).

canon(Expr, semi_stmt(Stmts)) :-
    canon_stmt(Expr, Stmts).

% Example:
%   let x = f (g 1) in
%   let y = begin
%       let z = f x in
%       h z
%   end in
%   y
%
% to
%
%   let t1 = g 1 in
%   let x = f t1 in
%   let z = f x in
%   let y = h z in
%   y

%canon([
%      	let_stmt(x, call(f, call(g, int(1)))),
%        let_stmt(y, [
%                    let_stmt(z, call(f, x)),
%                      call(h, z)
%                    ]),
%        y
%    ], Canon).
