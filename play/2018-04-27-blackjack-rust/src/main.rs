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

fn game() {
    println!("Game start.");

    // Decks.

    let mut deck: VecDeque<(String, i32)> = {
        let mut deck = Vec::new();
        for suite in &["♠", "🍀", "♥", "♦"] {
            {
                let name = format!("{}A", suite);
                deck.push((name, 1));
            }

            for i in 2..11 {
                let name = format!("{}{}", suite, i);
                deck.push((name, i));
            }

            for s in &["J", "Q", "K"] {
                let name = format!("{}{}", suite, s);
                deck.push((name, 10));
            }
        }

        let mut rng = rand::StdRng::new().unwrap();
        rng.shuffle(&mut deck);
        deck.into_iter().collect()
    };

    let mut dealer = 0;
    let mut you = 0;

    // Deal.

    let open_card = deck.pop_back().unwrap();
    let hidden_card = deck.pop_back().unwrap();
    dealer += open_card.1;
    println!("Dealer draws a hidden card.");
    println!("Dealer's open card is {}.", open_card.0);
    println!("Dealer's score is now {}.", dealer);

    // Your turn.

    loop {
        let card = deck.pop_back().unwrap();
        you += card.1;

        println!("You hit {}.", card.0);
        println!("Your total is now {}.", you);

        if you > 21 {
            println!("Bust! Dealer wins.");
            return;
        }

        println!("Hit or stand? (H/S)");
        let hit = read_line().starts_with("H");

        if !hit {
            break;
        }
    }

    // Dealer's turn.

    dealer += hidden_card.1;
    println!("Dealer's hidden card was {}.", hidden_card.0);
    println!("Dealer's total is {}.", dealer);

    while dealer < 17 {
        let card = deck.pop_front().unwrap();
        dealer += card.1;

        println!("Dealer receives {}.", card.1);
        println!("Dealer's total grows to {}.", dealer);
    }

    if dealer > 21 {
        println!("Dealer busts. You win!");
    }

    // Judge.

    let win = you > dealer;
    if win {
        println!("You win!");
    } else {
        println!("Dealer wins.");
    }
}

fn main() {
    println!("Welcome to Blackjack!");

    loop {
        game();

        println!("Press enter to restart.");
        read_line();
    }
}
