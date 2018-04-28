
% トランプ

ランク(エース).
ランク(2).
ランク(3).
ランク(4).
ランク(5).
ランク(6).
ランク(7).
ランク(8).
ランク(9).
ランク(10).
ランク(ジャック).
ランク(クイーン).
ランク(キング).

スート(スペード).
スート(クローバー).
スート(ハート).
スート(ダイア).

カード((Suit, Rank)) :-
    スート(Suit), ランク(Rank).

カードリスト(Cards) :-
    findall(Card, カード(Card), Cards).

デッキを生成する(Deck) :-
    カードリスト(Cards),
    random_permutation(Cards, Deck).

カードを引く([], (Card, Deck)) :-
    デッキを生成する([Card | Deck]).

カードを引く([Card | Deck], (Card, Deck)).



% スコアの計算

ランクのスコア(エース, 1).
ランクのスコア(エース, 11).
ランクのスコア(2, 2).
ランクのスコア(3, 3).
ランクのスコア(4, 4).
ランクのスコア(5, 5).
ランクのスコア(6, 6).
ランクのスコア(7, 7).
ランクのスコア(8, 8).
ランクのスコア(9, 9).
ランクのスコア(10, 10).
ランクのスコア(ジャック, 10).
ランクのスコア(クイーン, 10).
ランクのスコア(キング, 10).

手札のスコアの候補([], 0).
手札のスコアの候補([(_, Rank) | Hand], Score) :-
    手札のスコアの候補(Hand, Score1),
    ランクのスコア(Rank, RankScore),
    Score is Score1 + RankScore.

% 21を超えない組み合わせがあるなら、その中での最大値を手役とする。
手役(Hand, (nonbust, Score)) :-
    手札のスコアの候補(Hand, Score),
    Score =< 21,
    \+ (手札のスコアの候補(Hand, Score2), Score2 =< 21, Score2 > Score),
    !.

% 21を超えない組み合わせがないなら、その中での最小値を手役とする。
手役(Hand, (bust, Score)) :-
    手札のスコアの候補(Hand, Score),
    Score > 21,
    \+ (手札のスコアの候補(Hand, Score2), Score2 > Score).

手札のスコア(Hand, Score) :-
    手役(Hand, (_, Score)).

バーストしていない(Hand) :-
    手役(Hand, (nonbust, _)).



% UI

confirm(Message) :-
    write(Message),
    write(' (y/n)'),
    flush_output,
    get_char(Char),
    skip('\n'),
    Char = y.

ページ送り :-
    write(' ...'),
    flush_output,
    skip('\n'),
    nl.

カードを表示する((Suit, Rank)) :-
    write(Suit),
    write('の'),
    write(Rank).

アップカードを表示する(UpCard) :-
    write('アップカード: '),
    カードを表示する(UpCard),
    ページ送り.

ホールカードを表示する(Hand, HoleCard) :-
    手札のスコア(Hand, Score),
    write('ホールカード: '),
    writeln(HoleCard),
    write('ディーラーのスコア:'),
    write(Score),
    ページ送り.

ヒットしたカードを表示する(Card) :-
    write('ヒット: '),
    カードを表示する(Card),
    ページ送り.

プレイヤーのステータスを表示する(Hand) :-
    手札のスコア(Hand, Score),
    write('手札: '),
    writeln(Hand),
    write('スコア: '),
    write(Score),
    ページ送り.

ディーラーのステータスを表示する(Hand) :-
    手札のスコア(Hand, Score),
    write('ディーラーの手札: '),
    writeln(Hand),
    write('ディーラーのスコア:'),
    write(Score),
    ページ送り.

始まりの挨拶をする :-
    write('ブラックジャックへようこそ！'),
    ページ送り.

結果を表示する(Result) :-
    結果のメッセージ(Result, Message),
    write(Message),
    ページ送り.

結果のメッセージ(you_bust, 'バーストしました。ディーラーの勝ちです。').
結果のメッセージ(dealer_bust, 'バーストしました。あなたの勝ちです。').
結果のメッセージ(dealer_win, 'ディーラーの勝ちです。').
結果のメッセージ(you_win, 'あなたの勝ちです。').

別れの挨拶をする :-
    writeln('また遊んでね！').



% ゲームルーチン

ブラックジャックで遊ぶ :-
    ディーラーの初手を配る([], (Deck2, UpCard)),
    プレイヤーのターンを開始する(Deck2, (Deck3, PlayerHand)),
    バーストを確認する(PlayerHand, you_bust, continue, Flow1),
    ディーラーのターンを開始する(Deck3, [UpCard], Flow1, (_, DealerHand)),
    バーストを確認する(DealerHand, dealer_bust, Flow1, Flow2),
    スコアを比較する(DealerHand, PlayerHand, Flow2, finish(Result)),
    結果を表示する(Result).

バーストを確認する(_, _, finish(Result), finish(Result)).

バーストを確認する(Hand, _, continue, continue) :-
    バーストしていない(Hand), !.

バーストを確認する(_, Result, continue, finish(Result)).

ディーラーの初手を配る(Deck1, (Deck, UpCard)) :-
    カードを引く(Deck1, (UpCard, Deck)),
    アップカードを表示する(UpCard).

プレイヤーのターンを開始する(Deck1, (Deck, Hand)) :-
    write('あなたのターンです。'),
    ページ送り,
    ヒットする([], Deck1, (Deck2, Hand1)),
    プレイヤーのターンを継続する(Deck2, Hand1, (Deck, Hand)).

プレイヤーのターンを継続する(Deck1, Hand1, (Deck, Hand)) :-
    プレイヤーのステータスを表示する(Hand1),
    プレイヤーはヒットしてもよい(Deck1, Hand1, (Deck, Hand)), !.

プレイヤーのターンを継続する(Deck, Hand, (Deck, Hand)).

プレイヤーはヒットしてもよい(Deck1, Hand1, (Deck, Hand)) :-
    バーストしていない(Hand1),
    confirm('ヒットしますか？'),
    !,
    ヒットする(Hand1, Deck1, (Deck2, Hand2)),
    プレイヤーのターンを継続する(Deck2, Hand2, (Deck, Hand)).

ヒットする(Hand1, Deck1, (Deck, [Card | Hand1])) :-
    カードを引く(Deck1, (Card, Deck)),
    ヒットしたカードを表示する(Card).

ディーラーのターンを開始する(Deck, Hand, finish(_), (Deck, Hand)).

ディーラーのターンを開始する(Deck1, Hand1, continue, (Deck, Hand)) :-
    write('ディーラーのターンです。'),
    ページ送り,
    ホールカードをめくる(Deck1, Hand1, (Deck2, Hand2)),
    ディーラーは可能ならヒットする(Deck2, Hand2, (Deck, Hand)).

ホールカードをめくる(Deck1, Hand1, (Deck, Hand)) :-
    カードを引く(Deck1, (HoleCard, Deck)),
    Hand = [HoleCard | Hand1],
    ホールカードを表示する(Hand, HoleCard).

ディーラーは可能ならヒットする(Deck1, Hand1, (Deck, Hand)) :-
    ディーラーがヒットできる(Hand1),
    ディーラーがヒットする(Deck1, Hand1, (Deck2, Hand2)),
    ディーラーのステータスを表示する(Hand2),
    ディーラーは可能ならヒットする(Deck2, Hand2, (Deck, Hand)).

ディーラーは可能ならヒットする(Deck, Hand, (Deck, Hand)).

ディーラーがヒットできる(Hand) :-
    手札のスコア(Hand, Score),
    Score < 17.

ディーラーがヒットする(Deck1, Hand1, (Deck, [Card | Hand1])) :-
    カードを引く(Deck1, (Card, Deck)),
    ヒットしたカードを表示する(Card).

スコアを比較する(_, _, finish(Result), finish(Result)).

スコアを比較する(DealerHand, PlayerHand, continue, finish(you_win)) :-
    手札のスコア(DealerHand, DealerScore),
    手札のスコア(PlayerHand, PlayerScore),
    PlayerScore > DealerScore, !.

スコアを比較する(_, _, continue, finish(dealer_win)).



main :-
    始まりの挨拶をする,
    ブラックジャックで遊ぶ,
    別れの挨拶をする.
