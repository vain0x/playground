
% Memo:
% sudo docker run -it swipl
% ?- [user].
% <input rules>
% ^D

confirm(Message) :-
    write(Message),
    write(' (y/n)'),
    flush_output,
    get_char(Char),
    skip('\n'),
    Char = y.

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

ランクのスコア(エース, 1).
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

スコア(Cards, Score) :-
    スコア_loop(Cards, 0, Score).

スコア_loop([], Score, Score).

スコア_loop([(_, Rank)|Cards], ScoreAcc, Score) :-
    ランクのスコア(Rank, RankScore),
    ScoreAcc2 is ScoreAcc + RankScore,
    スコア_loop(Cards, ScoreAcc2, Score).



ブラックジャックで遊ぶ(Result) :-
    デッキを生成する(Deck),
    最初の2枚を配る(Deck, Result).

デッキを生成する(Deck) :-
    findall(Card, カード(Card), Cards),
    random_permutation(Cards, Deck).

最初の2枚を配る([HiddenCard, OpenCard | Deck], Result) :-
    GameState = (HiddenCard, OpenCard, Result),
    ディーラーの公開カードを表示する(OpenCard),
    プレイヤーのターンを開始する(Deck, GameState).

ディーラーの公開カードを表示する(OpenCard) :-
    write('ディーラーの公開カード: '),
    writeln(OpenCard).

プレイヤーのターンを開始する(Deck, GameState) :-
    writeln('あなたのターンです。'),
    ヒットする([], Deck, GameState).

ヒットする(Hand, [Card | Deck], GameState) :-
    Hand2 = [Card | Hand],
    write('ヒット: '),
    writeln(Card),
    プレイヤーのターンに戻る([Card | Hand], Deck, GameState).

プレイヤーのターンに戻る(Hand, Deck, GameState) :-
    プレイヤーのステータスを表示する(Hand),
    プレイヤーのバーストを確認する(Hand, Deck, GameState).

プレイヤーのステータスを表示する(Hand) :-
    スコア(Hand, Score),
    write('手札: '),
    writeln(Hand),
    write('スコア: '),
    writeln(Score).

プレイヤーのバーストを確認する(Hand, Deck, (_, _, you_bust)) :-
    スコア(Hand, Score),
    Score > 21.

プレイヤーのバーストを確認する(Hand, Deck, GameState) :-
    GameState = (_, _, Result),
    スコア(Hand, Score),
    Score =< 21,
    プレイヤーが選択する(Hand, Deck, GameState).

プレイヤーが選択する(Hand, Deck, GameState) :-
    confirm('ヒットしますか？'), !,
    ヒットする(Hand, Deck, GameState).

プレイヤーが選択する(Hand, Deck, GameState) :-
    ディーラーのターン(Hand, Deck, GameState).

ディーラーのターン(PlayerHand, Deck, GameState) :-
    GameState = (HiddenCard, OpenCard, Result),
    Hand = [HiddenCard, OpenCard],
    writeln('ディーラーのターンです。'),
    ディーラーの非公開カードを公開する(Hand, HiddenCard),
    ディーラーは可能なかぎりヒットする(Hand, Deck, (PlayerHand, Result)).

ディーラーの非公開カードを公開する(Hand, HiddenCard) :-
    スコア(Hand, Score),
    write('ディーラーの非公開カード: '),
    writeln(HiddenCard),
    write('ディーラーのスコア:'),
    writeln(Score).

ディーラーは可能なかぎりヒットする(Hand, Deck, GameState) :-
    ディーラーはヒットする必要がある(Hand), !,
    ディーラーがヒットする(Hand, Deck, GameState).

ディーラーは可能なかぎりヒットする(Hand, _, (PlayerHand, Result)) :-
    ディーラーのバーストを確認する(Hand, PlayerHand, Result).

ディーラーはヒットする必要がある(Hand) :-
    スコア(Hand, Score),
    Score < 17.

ディーラーがヒットする(Hand, [Card | Deck], GameState) :-
    write('ヒット: '),
    writeln(Card),
    Hand2 = [Card | Hand],
    ディーラーのステータスを表示する(Hand2),
    ディーラーは可能なかぎりヒットする(Hand2, Deck, GameState).

ディーラーのステータスを表示する(Hand) :-
    スコア(Hand, Score),
    write('ディーラーの手札: '),
    writeln(Hand),
    write('ディーラーのスコア:'),
    writeln(Score).

ディーラーのバーストを確認する(Hand, _, dealer_bust) :-
    スコア(Hand, Score),
    Score > 21.

ディーラーのバーストを確認する(Hand, PlayerHand, Result) :-
    スコア(Hand, Score),
    Score =< 21,
    スコアを比較する(Hand, PlayerHand, Result).

スコアを比較する(DealerHand, PlayerHand, you_win) :-
    スコア(DealerHand, DealerScore),
    スコア(DealerHand, PlayerScore),
    PlayerScore > DealerScore, !.

スコアを比較する(_, _, dealer_win).

始まりの挨拶をする :-
    writeln('ブラックジャックへようこそ！').

結果を表示する(you_bust) :-
    writeln('バーストしました。ディーラーの勝ちです。').

結果を表示する(dealer_bust) :-
    writeln('バーストしました。あなたの勝ちです。').

結果を表示する(dealer_win) :-
    writeln('あなたの勝ちです。').

結果を表示する(you_win) :-
    writeln('ディーラーの勝ちです。').

別れの挨拶をする :-
    writeln('また遊んでね！').

main :-
    始まりの挨拶をする,
    ブラックジャックで遊ぶ(Result),
    結果を表示する(Result),
    別れの挨拶をする.
