% Bit operations.

bit_complement(0, 1).
bit_complement(1, 0).

complement([], []).

complement([X | Xs], [Y | Ys]) :-
    bit_complement(X, Y),
    complement(Xs, Ys).

add_total((0, 0, 0), (0, 0)).
add_total((1, 0, 0), (1, 0)).
add_total((0, 1, 0), (1, 0)).
add_total((0, 0, 1), (1, 0)).
add_total((1, 1, 0), (0, 1)).
add_total((1, 0, 1), (0, 1)).
add_total((0, 1, 1), (0, 1)).
add_total((1, 1, 1), (1, 1)).



% i64

%% i64_from_number(Bits, N)
%% <=> Bits is a 64-bit list representing N as complementary of 2.
%% N may not be free var.

i64_from_number(Bits, N) :-
    N >= 0,
    length(Bits, 8),
    i64_from_number_loop(Bits, N).

i64_from_number(BitXs, N) :-
    N < 0,
    M is -N - 1,
    i64_from_number(BitYs, M),
    complement(BitYs, BitXs).

i64_from_number_loop([0], 0).

i64_from_number_loop([0, Bit | Bits], 0) :-
    i64_from_number_loop([Bit | Bits], 0).

i64_from_number_loop([Bit | Bits], N) :-
    N > 0,
    M is div(N, 2),
    Bit is mod(N, 2),
    i64_from_number_loop(Bits, M).

%% i64_to_number(Bits, N)
%% e.g. i64_to_number([1, 1, 0, ...], 3).
%% Bits may not be free var.

i64_to_number(Bits, N) :-
    reverse(Bits, Rev),
    i64_to_number_core(Rev, 0, N).

i64_to_number_core([0 | Xs], 0, N) :-
    i64_to_number_loop([0 | Xs], 0, N).

i64_to_number_core([1 | Xs], 0, N) :-
    complement([1 | Xs], Ys),
    i64_to_number_loop(Ys, 0, M),
    N is -M - 1.

i64_to_number_loop([], N, N).

i64_to_number_loop([B | Bs], M, N) :-
    M2 is M * 2 + B,
    i64_to_number_loop(Bs, M2, N).

%% add(L, R, Sum) <=> L + R = Sum

add(L, R, Sum) :-
    add_loop(0, L, R, Sum).

add_loop(C, [X], [Y], [Z]) :-
    add_total((X, Y, C), (Z, _)).

add_loop(C, [X1, X2 | Xs], [Y1, Y2 | Ys], [Z1, Z2 | Zs]) :-
    add_total((X1, Y1, C), (Z1, C2)),
    add_loop(C2, [X2 | Xs], [Y2 | Ys], [Z2 | Zs]).

%% sub(L, R, Diff) <=> L - R = Diff

sub(L, R, Diff) :-
    add(L, Diff, R).

%% left_shift(L, K, Shifted) <=> Shifted = L * 2^K
%% K: nat

left_shift(Bits, K, Shifted) :-
    nat_length(Bits, L),
    nat_sub(L, K, N),
    left_shift_loop(Bits, N, K, Shifted).

left_shift_loop(_, 0, 0, []).

left_shift_loop(Bits, N, succ(K), [0 | Shifted]) :-
    left_shift_loop(Bits, N, K, Shifted).

left_shift_loop([B | Bits], succ(N), 0, [B | Shifted]) :-
    left_shift_loop(Bits, N, 0, Shifted).

%% mul(L, R, Multiple) <=> Multiple = L * R

mul(L, R, Prod) :-
    i64_from_number(S, 0),
    mul_loop(L, R, 0, S, Prod).

% mul_loop(X, Y, K, S, Prod) :- Prod = 2^K * X * Y + S.
% 2^k(2x + a)(2y + b) + s
% = 2^k(4xy + 2bx + 2ay + ab) + s
% = 2^(k + 2) xy + (2^(k + 1) bx + 2^(k + 1) ay + 2^k ab + s)

mul_loop([A], [B], K, S1, Prod) :-
    bit_and(A, B, AB),
    i64_from_number(_1, 1),
    mul_next(K, AB, _1, S1, Prod).

mul_loop([A, X1 | X], [B, Y1 | Y], K, S1, Prod) :-
    bit_and(A, B, AB),
    %p('X', [A, X1 | X]),
    %p('Y', [B, Y1 | Y]),
    %p('S', S1),
    mul_next(succ(K), B, [X1 | X], S1, S2),
    mul_next(succ(K), A, [Y1 | Y], S2, S3),
    i64_from_number(_1, 1),
    mul_next(K, AB, _1, S3, S4),
    mul_loop([X1 | X], [Y1 | Y], succ(succ(K)), S4, Prod).

% mul_next(K, B, X, S, T)
% <=> T = S + (B = 1 ? 2^K * X : 0)

mul_next(_, 0, _, S, S).

mul_next(K, 1, X, S, T) :-
    left_shift(X, K, Y),
    int_extend(Y, S, Yx, Sx),
    add(Yx, Sx, T).

% 符号拡張

int_extend([X], [Y], [X], [Y]).

int_extend([X], [Y1, Y2 | Ys], [X, X | RX], [Y1, Y2 | Ys]) :-
    int_extend([X], [Y2 | Ys], [X | RX], [Y2 | Ys]).

int_extend([X1, X2 | Xs], [Y], RX, RY) :-
    int_extend([Y], [X1, X2 | Xs], RY, RX).

int_extend([X1, X2 | Xs], [Y1, Y2 | Ys], [X1, X2 | RX], [Y1, Y2 | RY]) :-
    int_extend([X2 | Xs], [Y2 | Ys], [X2 | RX], [Y2 | RY]).



p(L, N) :-
    i64_to_number(N, NN),
    write(L),
    write(' = '),
    writeln(NN).


positive([0]).
positive([_|X]) :- positive(X).

sample_add :-
    i64_from_number(_2, 2),
    i64_from_number(_3, 3),
    i64_from_number(_6, 6),
    add(_2, _3, N),
    i64_to_number(N, NN).

sample_nondet(B, AN) :-
    i64_from_number(_2, 2),
    i64_from_number(_3, 3),
    i64_from_number(_6, 6),
    add(A, B, _6),
    positive(A),
    i64_to_number(A, AN).
