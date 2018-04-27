//! # Blackjack written in Rust
//! [](https://qiita.com/hirossyi73/items/cf8648c31898216312e5)

//! ## Rule
//! - A player (You) vs. dealer (Computer)
//! - A deck initially consists of 52 cards.
//! - Firstly the dealer draws 2 cards and reveals one of them.
//! - Player's turns:
//!     - Draw a card.
//!     - You "bust" if your total score is larger than 21.
//!     - Otherwise, you may repeat this.
//! - Then the dealer draws a card while their total score is less than 17. If the score exceeds 21, they busts.
//! - Finally you win if and only if your total score is higher than that of the dealer.

extern crate rand;

use rand::Rng;
use std::collections::VecDeque;

fn read_line() -> String {
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim().to_owned()
}

type Score = i32;

struct Deck {
    cards: VecDeque<Card>,
    rng: rand::StdRng,
}

impl Deck {
    fn unseal_and_shuffle(rng: &mut rand::StdRng) -> VecDeque<Card> {
        let mut cards = Card::all();
        rng.shuffle(&mut cards);
        VecDeque::from(cards)
    }

    pub fn new() -> Deck {
        let mut rng = rand::StdRng::new().unwrap();
        let cards = Deck::unseal_and_shuffle(&mut rng);
        Deck { cards, rng }
    }

    pub fn draw(&mut self) -> Card {
        if self.cards.is_empty() {
            self.cards = Deck::unseal_and_shuffle(&mut self.rng);
        }

        self.cards.pop_front().unwrap()
    }
}

struct Player {
    name: String,
    score: Score,
}

impl Player {
    pub fn is_bust(&self) -> bool {
        self.score > 21
    }

    pub fn receive(&mut self, card: &Card) {
        self.score += card.score();
    }
}

struct Card {
    rank: Rank,
    suit: Suit,
}

impl Card {
    pub fn all() -> Vec<Card> {
        let mut cards = Vec::new();
        for suit in Suit::all() {
            for rank in Rank::all() {
                cards.push(Card { suit, rank });
            }
        }
        cards
    }

    pub fn score(&self) -> Score {
        self.rank.score()
    }

    pub fn to_string(&self) -> String {
        format!("{}{}", self.rank.to_string(), self.suit.to_string())
    }
}

#[derive(Clone, Copy)]
enum Rank {
    Ace,
    Number(i32),
    Jack,
    Queen,
    King,
}

impl Rank {
    pub fn all() -> Vec<Rank> {
        let mut ranks = Vec::new();
        ranks.push(Rank::Ace);
        for i in 2..11 {
            ranks.push(Rank::Number(i));
        }
        ranks.push(Rank::Jack);
        ranks.push(Rank::Queen);
        ranks.push(Rank::King);
        ranks
    }

    pub fn score(&self) -> Score {
        match *self {
            Rank::Ace => 1,
            Rank::Number(value) => value,
            Rank::Jack | Rank::Queen | Rank::King => 10,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Rank::Ace => "A".to_string(),
            Rank::Number(n) => n.to_string(),
            Rank::Jack => "J".to_string(),
            Rank::Queen => "Q".to_string(),
            Rank::King => "K".to_string(),
        }
    }
}

#[derive(Clone, Copy)]
enum Suit {
    Space,
    Clover,
    Heart,
    Diamond,
}

impl Suit {
    pub fn all() -> Vec<Suit> {
        vec![Suit::Space, Suit::Clover, Suit::Heart, Suit::Diamond]
    }

    pub fn to_string(self) -> &'static str {
        match self {
            Suit::Space => "♠",
            Suit::Clover => "🍀",
            Suit::Heart => "♥",
            Suit::Diamond => "♦",
        }
    }
}

struct BlackjackGame {
    you: Player,
    dealer: Player,
    deck: Deck,
}

impl BlackjackGame {
    fn new() -> BlackjackGame {
        BlackjackGame {
            you: Player {
                name: "You".to_owned(),
                score: 0,
            },
            dealer: Player {
                name: "Dealer".to_owned(),
                score: 0,
            },
            deck: Deck::new(),
        }
    }

    pub fn run() {
        let mut game = BlackjackGame::new();

        println!("Game start.");
        println!("--------------------");

        // Deal.

        let open_card = game.deck.draw();
        let hidden_card = game.deck.draw();
        game.dealer.receive(&open_card);
        println!("Dealer draws a hidden card.");
        println!("Dealer's open card is {}.", open_card.to_string());
        println!("Dealer's score is now {}.", game.dealer.name);

        // Your turn.

        loop {
            let card = game.deck.draw();
            game.you.receive(&card);

            println!("You hit {}.", card.to_string());
            println!("Your total is now {}.", game.you.score);

            if game.you.is_bust() {
                println!("Bust! Dealer wins.");
                return;
            }

            println!("Hit or stand? (H/S)");
            let hit = read_line().to_lowercase().starts_with("h");
            if !hit {
                break;
            }
        }

        // Dealer's turn.

        game.dealer.receive(&hidden_card);
        println!("Dealer's hidden card was {}.", hidden_card.to_string());
        println!("Dealer's total is {}.", game.dealer.score);

        while game.dealer.score < 17 {
            let card = game.deck.draw();
            game.dealer.receive(&card);

            println!("Dealer receives {}.", card.to_string());
            println!("Dealer's total grows to {}.", game.dealer.score);
        }

        if game.dealer.is_bust() {
            println!("Dealer busts. You win!");
        }

        // Judge.

        let win = game.you.score > game.dealer.score;
        if win {
            println!("You win!");
        } else {
            println!("Dealer wins.");
        }
    }
}

fn game() {
    BlackjackGame::run();
}

fn main() {
    println!("Welcome to Blackjack!");

    loop {
        game();

        println!("Next? (y/n)");
        let yes = read_line().to_lowercase().starts_with("y");
        if !yes {
            println!("Thanks for playing.");
            return;
        }
    }
}
