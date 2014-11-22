#![feature(tuple_indexing)]

use std::rand::{random};


#[deriving(Show, PartialEq, PartialOrd, Eq, Ord, Rand, Clone)]
pub enum Rank {
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    Jack,
    Queen,
    King,
    Ace,
}

impl Rank {
    fn value(&self) -> uint {
        match *self {
            R2 => 2, R3 => 3, R4 => 4, R5 => 5, R6 => 6, R7 => 7, R8 => 8, R9 => 9, R10 => 10,
            Jack => 11, Queen => 12, King => 13, Ace => 14
        }
    }
}


#[deriving(Show, PartialEq, PartialOrd, Eq, Ord, Rand, Clone)]
pub enum Suit {
    Clubs,
    Hearts,
    Diamonds,
    Spades,
}


#[deriving(Show, PartialEq, PartialOrd, Eq, Ord, Rand, Clone)]
pub struct Card {
    pub rank: Rank,
    pub suit: Suit,
}


#[deriving(Show, PartialEq, PartialOrd, Eq, Ord, Rand, Clone)]
pub enum Hand {
    High((Rank, Rank, Rank, Rank, Rank)),
    Pair1((Rank, Rank, Rank, Rank)),
    Pair2((Rank, Rank, Rank)),
    Kind3((Rank, Rank, Rank)),
    Straight(Rank),
    Flush((Rank, Rank, Rank, Rank, Rank)),
    FHouse((Rank, Rank)),
    Kind4((Rank, Rank)),
    SFlush(Rank),
}


#[deriving(Show, PartialEq, PartialOrd, Eq, Ord, Clone)]
pub struct Table {
    pub flop: (Card, Card, Card),
    pub turn: Card,
    pub river: Card,
}


pub type PHand = [Card, ..2];
pub type HandSet = [Card, ..5];


pub fn deal() -> (Table, [PHand, ..9]) {
    // The range of constants.
    let suits: [Suit, ..4] = [Suit::Clubs, Suit::Hearts, Suit::Diamonds, Suit::Spades];
    let ranks: [Rank, ..13] = [R2, R3, R4, R5, R6, R7, R8, R9, R10, Jack, Queen, King, Ace];
    // Shuffle indices over an ordered deck.
    let mut deck_idx = [(0u,0u), ..52];
    for i in range(0u, 52u) {
        deck_idx[i] = (random(), i);
    }
    deck_idx.sort();
    // Indexing function for clarity.
    let idx = |i: uint| {
        let j = deck_idx[i].1;
        Card {
            rank: ranks[j % 13],
            suit: suits[j / 13],
        }
    };
    // Fill up table, end with 4 offset.
    let table = Table {
            flop: (idx(0), idx(1), idx(2)),
            river: idx(3),
            turn: idx(4),
    };
    // Fill up player hands, start with 5 offset.
    let mut phands = [[Card { rank: R2, suit: Clubs }, ..2], ..9]; // Init with junk card values.
    for i in range(0u, 9u) {
        let ofs = 5 + i * 2;
        phands[i] = [idx(ofs), idx(ofs + 1)]; 
    }
    (table, phands)
}


fn best_hand_aux_s_flush(cards: &[Card, ..7]) -> Option<(Hand, HandSet)> {
    // Resort by suit then by ranks.
    let cards = {
        let mut cards = *cards.clone();
        cards.sort_by(|a,b|
            match b.suit.cmp(&a.suit) {
                Less => Less,
                Greater => Greater,
                Equal => b.rank.cmp(&a.rank),
            }
        );
        cards
    };
    // Find boundaries for at least five similar suits.
    let bounds = || {
        for i in range(0u, cards.len() - 4) {
            let mut same_suit = 0;
            for j in range(i + 1, cards.len()) {
                if cards[i].suit == cards[j].suit {
                    same_suit += 1;
                }
            }
            if same_suit >= 4 {
                return Some((i, i + same_suit));
            }
        }
        None
    };
    bounds()
    .and_then(|(start, end)|
        best_hand_aux_straight_n(&cards, start, end)
        .or_else(|| best_hand_aux_straight_n_low(&cards, start, end))
        .map(|(rank, hand_set)| (SFlush(rank), hand_set)))
}


// Pre-cond: descendingly sorted cards.
fn best_hand_aux_kind_4(cs: &[Card, ..7]) -> Option<(Hand, HandSet)> {
    // Find four consecutive similar ranks.
    let mut j = None;
    for i in range(0u, cs.len() - 3) {
        if cs[i].rank == cs[i + 1].rank && cs[i].rank == cs[i + 2].rank {
            j = Some(i);
            break;
        }
    }
    // Not found.
    let j = match j {
        Some(j) => j,
        None => return None
    };
    // Kicker's index.
    let mut k = 0u;
    for i in range(0u, cs.len()) {
        if i == j || i == j + 1 || i == j + 2 || i == j + 3{
            continue;
        }
        k = i;
        break;
    }
    let hand = Kind4((cs[j].rank, cs[k].rank));
    let hand_set = [cs[j], cs[j + 1], cs[j + 2], cs[j + 3], cs[k]];
    Some((hand, hand_set))
}


// Pre-cond: descendingly sorted cards.
fn best_hand_aux_f_house(cs: &[Card, ..7]) -> Option<(Hand, HandSet)> {
    // Find three consecutive similar ranks.
    let mut j = None;
    for i in range(0u, cs.len() - 2) {
        if cs[i].rank == cs[i + 1].rank && cs[i].rank == cs[i + 2].rank {
            j = Some(i);
            break;
        }
    }
    // Not found.
    let j = match j {
        Some(j) => j,
        None => return None
    };
    // Find two consecutive similar ranks.
    let mut k = None;
    for i in range(0u, cs.len() - 1) {
        if i < j && i > j + 2 && cs[i].rank == cs[i + 1].rank {
            k = Some(i);
            break;
        }
    }
    // Not found.
    let k = match k {
        Some(k) => k,
        None => return None
    };
    let hand = FHouse((cs[j].rank, cs[k].rank));
    let hand_set = [cs[j], cs[j + 1], cs[j + 2], cs[k], cs[k + 1]];
    Some((hand, hand_set))
}


fn best_hand_aux_flush(cards: &[Card, ..7]) -> Option<(Hand, HandSet)> {
    // Resort by suit then by ranks.
    let mut cs: [Card, ..7] = *cards.clone();
    cs.sort_by(|a,b| match b.suit.cmp(&a.suit) {
            Less => Less,
            Greater => Greater,
            Equal => b.rank.cmp(&a.rank),
        }
    );
    let mut j = None;
    for i in range(0u, cs.len() - 4) {
        if cs[i].suit == cs[i + 1].suit &&
                    cs[i].suit == cs[i + 2].suit &&
                    cs[i].suit == cs[i + 3].suit &&
                    cs[i].suit == cs[i + 4].suit {
            j = Some(i);
            break;
        }
    }
    // Not found.
    let j = match j {
        Some(j) => j,
        None => return None
    };
    let hand = Flush((cs[j].rank, cs[j + 1].rank, cs[j + 2].rank, cs[j + 3].rank, cs[j + 4].rank));
    let hand_set = [cs[j], cs[j + 1], cs[j + 2], cs[j + 3], cs[j + 4]];
    Some((hand, hand_set))
}


fn best_hand_aux_straight_n_low(cs: &[Card, ..7], start: uint, end: uint)
        -> Option<(Rank, HandSet)> {
    let mut idxs: [Option<uint>, ..5] = [None, ..5];
    for i in range(start, end) {
        match cs[i].rank {
            R5 => { idxs[0] = Some(i); },
            R4 => { idxs[1] = Some(i); },
            R3 => { idxs[2] = Some(i); },
            R2 => { idxs[3] = Some(i); },
            Ace => { idxs[4] = Some(i); },
            _ => {}
        }
        let mut filled = true;
        for &idx in idxs.iter() {
            filled &= idx != None;
        }
        if filled {
            break;
        }

    }
    idxs[0].and_then(|r5|
    idxs[1].and_then(|r4|
    idxs[2].and_then(|r3|
    idxs[3].and_then(|r2|
    idxs[4].and_then(|ace|
    Some((R5, [cs[r5], cs[r4], cs[r3], cs[r2], cs[ace]])))))))
}


// Pre-cond: descendingly sorted cards.
fn best_hand_aux_straight_n(cs: &[Card, ..7], start: uint, end: uint) -> Option<(Rank, HandSet)> {
    // Scan for five consecutively descending ranks. And skip equal ranks!
    let mut idxs: [uint, ..5] = [0u, ..5];
    let mut ofs = 0;
    for i in range(start, end - 4) {
        ofs = 0;
        for j in range(i, end - 1) {
            let is_eq = cs[j].rank == cs[j + 1].rank;
            let is_succ = cs[j].rank.value() == cs[j + 1].rank.value() + 1;
            match (is_eq, is_succ) {
                (true, false) => {}
                (false, false) => { ofs = 0; }
                (_, true) => {
                    idxs[ofs] = j;
                    idxs[ofs + 1] = j + 1;
                    ofs += 1;
                },
            }
            if ofs == 4 {
                break;
            }
        }
        if ofs == 4 {
            break;
        }
    }
    // Not found.
    if ofs != 4 {
        return None;
    }
    let rank = cs[idxs[0]].rank;
    let hand_set = [cs[idxs[0]], cs[idxs[1]], cs[idxs[2]], cs[idxs[3]], cs[idxs[4]]];
    Some((rank, hand_set))
}


// Pre-cond: descendingly sorted cards.
fn best_hand_aux_straight(cs: &[Card, ..7]) -> Option<(Hand, HandSet)> {
    best_hand_aux_straight_n(cs, 0, 7).or_else(|| best_hand_aux_straight_n_low(cs, 0, 7))
        .map(|(rank, hand_set)| (Straight(rank), hand_set))
}


// Pre-cond: descendingly sorted cards.
fn best_hand_aux_kind_3(cs: &[Card, ..7]) -> Option<(Hand, HandSet)> {
    // Find three consecutive similar ranks.
    let mut j = None;
    for i in range(0u, cs.len() - 2) {
        if cs[i].rank == cs[i + 1].rank && cs[i].rank == cs[i + 2].rank {
            j = Some(i);
            break;
        }
    }
    // Not found.
    let j = match j {
        Some(j) => j,
        None => return None
    };
    // Kickers' indices.
    let mut k = [0u, ..2];
    let mut m = 0u;
    for i in range(0u, cs.len()) {
        if i == j || i == j + 1 || i == j + 2 {
            continue;
        }
        k[m] = i;
        m += 1;
        if m == k.len() {
            break;
        }
    }
    let hand = Kind3((cs[j].rank, cs[k[0]].rank, cs[k[1]].rank));
    let hand_set = [cs[j], cs[j + 1], cs[j + 2], cs[k[0]], cs[k[1]]];
    Some((hand, hand_set))
}


// Pre-cond: descendingly sorted cards.
fn best_hand_aux_pair_2(cs: &[Card, ..7]) -> Option<(Hand, HandSet)> {
    // Find a pair of two consecutive similar ranks.
    let mut j = None;
    let mut k = None;
    for i in range(0u, cs.len() - 1) {
        if cs[i].rank == cs[i + 1].rank {
            match j {
                None => j = Some(i),
                Some(_) => {
                    k = Some(i);
                    break;
                }
            }
        }
    }
    // Not found.
    let j = match j {
        Some(j) => j,
        None => return None
    };
    let k = match k {
        Some(k) => k,
        None => return None
    };
    // Kicker's index.
    let mut p = 0u;
    for i in range(0u, cs.len()) {
        if i == j || i == j + 1 {
            continue;
        }
        if i == k || i == k + 1 {
            continue;
        }
        p = i;
        break;
    }
    let hand = Pair2((cs[j].rank, cs[k].rank, cs[p].rank));
    let hand_set = [cs[j], cs[j + 1], cs[k], cs[k + 1], cs[p]];
    Some((hand, hand_set))
}


// Pre-cond: descendingly sorted cards.
fn best_hand_aux_pair_1(cards: &[Card, ..7]) -> Option<(Hand, HandSet)> {
    // Find two consecutive similar ranks.
    let mut j = None;
    for i in range(0u, cards.len() - 1) {
        if cards[i].rank == cards[i + 1].rank {
            j = Some(i);
            break;
        }
    }
    // Not found.
    let j = match j {
        Some(j) => j,
        None => return None
    };
    // Kickers' indices.
    let mut k = [0u, ..3];
    let mut m = 0u;
    for i in range(0u, cards.len()) {
        if i == j || i == j + 1 {
            continue;
        }
        k[m] = i;
        m += 1;
        if m == k.len() {
            break;
        }
    }
    let hand = Pair1((cards[j].rank, cards[k[0]].rank, cards[k[1]].rank, cards[k[2]].rank));
    let hand_set = [cards[j], cards[j + 1], cards[k[0]], cards[k[1]], cards[k[2]]];
    Some((hand, hand_set))
}


// Pre-cond: descendingly sorted cards.
fn best_hand_aux_high(cs: &[Card, ..7]) -> (Hand, HandSet) {
    (High((cs[0].rank, cs[1].rank, cs[2].rank, cs[3].rank, cs[4].rank)),
     [cs[0], cs[1], cs[2], cs[3], cs[4]])
}


pub fn best_hand(table: &Table, phand: &PHand) -> (Hand, HandSet) {
    let cards: &[Card, ..7] = &{
        let mut cards: [Card, ..7] = [
            table.flop.0,
            table.flop.1,
            table.flop.2,
            table.river,
            table.turn,
            phand[0],
            phand[1]
        ];
        cards.sort_by(|a,b| b.cmp(a)); // Descending sort.
        cards
    };

    best_hand_aux_s_flush(cards)
        .unwrap_or_else(|| best_hand_aux_kind_4(cards) 
            .unwrap_or_else(|| best_hand_aux_f_house(cards) 
                .unwrap_or_else(|| best_hand_aux_flush(cards) 
                    .unwrap_or_else(|| best_hand_aux_straight(cards) 
                        .unwrap_or_else(|| best_hand_aux_kind_3(cards) 
                            .unwrap_or_else(|| best_hand_aux_pair_2(cards)
                                .unwrap_or_else(|| best_hand_aux_pair_1(cards)
                                    .unwrap_or_else(|| best_hand_aux_high(cards)))))))))
}

/*
fn main() {
    let (table, phands) = deal();
    let (hand, hand_set) = best_hand(&table, &phands[0]);
    println!("{}", table);
    println!("phand: {}", phands[0].as_slice());
    println!("hand: {}", hand);
    println!("hand_set:{}", hand_set.as_slice());
}
*/
