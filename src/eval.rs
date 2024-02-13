//! Semantics of String Expressions P
//!
//! Corresponds to the Figure 2 of the paper

use std::{ops::Index, usize};

use crate::language::*;

/// holds values for m string variables v1, ..., vm
///
/// denoting the multiple input columns in a spreadsheet
#[derive(Debug)]
pub struct InputState(Vec<String>);

/// Option is used to represent the undefined value ⊥
pub fn eval(p: &StringExpr, sigma: &InputState) -> Option<String> {
    for (b, e) in p.iter() {
        if eval_bool(b, sigma) {
            return eval_trace(e, sigma);
        }
    }
    None
}

pub fn eval_bool(b: &Bool, sigma: &InputState) -> bool {
    for c in b.iter() {
        if eval_conjunct(c, sigma) {
            return true;
        }
    }
    false
}

pub fn eval_conjunct(c: &Conjunct, sigma: &InputState) -> bool {
    for p in c.iter() {
        if !eval_predicate(p, sigma) {
            return false;
        }
    }
    true
}

pub fn eval_predicate(p: &Predicate, sigma: &InputState) -> bool {
    match p {
        Predicate::Is(m) => eval_match(m, sigma),
        Predicate::Not(m) => !eval_match(m, sigma),
    }
}

pub fn eval_match(m: &Match, sigma: &InputState) -> bool {
    let s = &sigma[m.v];
    let r = &m.r;
    find_all_matches(r, s).collect::<Vec<_>>().len() >= m.k
}

pub fn eval_regular(r: &RegularExpr, s: &str) -> bool {
    // empty sequence + empty string
    if s.is_empty() {
        return r.is_empty();
    }
    let mut s = s.chars().peekable();
    for token in r.iter() {
        // exist other token to match but string end
        if s.peek().is_none() {
            return false;
        }
        match token {
            Token::Special(special) => {
                let c = s.next().unwrap();
                if !special.matched(&c) {
                    return false;
                }
            }
            Token::OneOrMore(token) => {
                // first character must matched, C+
                let c = s.next().unwrap();
                if !token.matched(&c) {
                    return false;
                }
                while s.next_if(|c| token.matched(c)).is_some() {}
            }
            Token::ExcludeOneOrMore(token) => {
                // first character must not matched, not C+
                let c = s.next().unwrap();
                if token.matched(&c) {
                    return false;
                }
                while s.next_if(|c| !token.matched(c)).is_some() {}
            }
        }
    }
    s.peek().is_none()
}

pub fn eval_trace(e: &TraceExpr, sigma: &InputState) -> Option<String> {
    e.iter().try_fold("".to_string(), |acc, f| {
        let s1 = eval_atomic(f, sigma)?;
        Some(acc + &s1)
    })
}

pub fn eval_atomic(f: &AtomicExpr, sigma: &InputState) -> Option<String> {
    match f {
        AtomicExpr::SubStr { v, p1, p2 } => {
            let p1 = eval_position(p1, &sigma[*v])?;
            let p2 = eval_position(p2, &sigma[*v])?;
            // Paper use [p1..p2] and [p1..=p2] mix!
            Some(sigma[*v][p1..p2].to_string())
        }
        AtomicExpr::ConstStr(s) => Some(s.clone()),
        AtomicExpr::Loop { e } => Some(eval_loop(e, 1, sigma)),
    }
}

fn substitute_trace(e: &TraceExpr, k: usize) -> TraceExpr {
    TraceExpr(e.iter().map(|f| substitute_atomic(f, k)).collect())
}

fn substitute_atomic(f: &AtomicExpr, k: usize) -> AtomicExpr {
    match f {
        AtomicExpr::SubStr { v, p1, p2 } => AtomicExpr::SubStr {
            v: *v,
            p1: substitute_position(p1, k),
            p2: substitute_position(p2, k),
        },
        AtomicExpr::ConstStr(_) => f.clone(),
        AtomicExpr::Loop { e: _ } => todo!("loop twice"),
    }
}

fn substitute_position(p: &Position, k: usize) -> Position {
    match p {
        Position::CPos(_) => p.clone(),
        Position::Pos { r1, r2, c } => {
            let c = match c {
                IntegerExpr::Constant(_) => c.clone(),
                IntegerExpr::Bound => IntegerExpr::Constant(k as i32),
            };
            Position::Pos {
                r1: r1.clone(),
                r2: r2.clone(),
                c,
            }
        }
    }
}

pub fn eval_loop(e: &TraceExpr, k: usize, sigma: &InputState) -> String {
    let t = substitute_trace(e, k);
    match eval_trace(&t, sigma) {
        Some(s) => s + eval_loop(e, k + 1, sigma).as_str(),
        None => "".to_string(),
    }
}

fn find_all_matches(r: &RegularExpr, s: &str) -> impl Iterator<Item = (isize, isize)> {
    if r.contains(&Token::Special(Special::Start)) {
        let mut r = r.clone();
        r.0.remove(0);
        if r.is_empty() {
            return vec![(0, -1)].into_iter();
        }
        unimplemented!("not only single startTok")
    } else if r.contains(&Token::Special(Special::End)) {
        let mut r = r.clone();
        r.0.remove(r.0.len() - 1);
        if r.is_empty() {
            return vec![(s.len() as isize, s.len() as isize - 1)].into_iter();
        }
        unimplemented!("not only single endTok");
    } else {
        if r.is_empty() {
            // return all possible range
            return (0..(s.len() + 1) as isize)
                .zip(-1..s.len() as isize)
                .collect::<Vec<_>>()
                .into_iter();
        }
        let mut v = vec![];
        let mut start = 0;
        'outer: while start < s.len() {
            for end in (start..s.len()).rev() {
                if eval_regular(r, &s[start..=end]) {
                    v.push((start as isize, end as isize));
                    start = end + 1;
                    continue 'outer;
                }
            }
            start += 1;
        }
        v.into_iter()
    }
}

pub fn eval_position(p: &Position, s: &str) -> Option<usize> {
    match p {
        Position::CPos(k) => {
            if *k >= 0 {
                Some(*k as usize)
            } else {
                // Paper says Length(s) + k, which is WRONG!
                Some(s.len() + 1 - (-k) as usize)
            }
        }
        Position::Pos { r1, r2, c } => {
            let k = if let IntegerExpr::Constant(i) = c {
                *i
            } else {
                panic!("IntegerExpe::Bound should be replaced!")
            };
            let mut matched_t = vec![];
            // t1 <= t3 or t3 = t1 - 1()
            // t  <= t2 or t2 = t - 1()
            for (_t1, t3) in find_all_matches(r1, s) {
                for (t, _t2) in find_all_matches(r2, s) {
                    println!("[{_t1}, {t3}], [{t}, {_t2}]");
                    if t3 == t - 1 {
                        matched_t.push(t as usize);
                    }
                }
            }
            matched_t.sort();
            match k.cmp(&0) {
                std::cmp::Ordering::Less => matched_t.get(matched_t.len() - (-k) as usize).copied(),
                std::cmp::Ordering::Equal => {
                    panic!("Cannot use IntegerConstant 0 in Position")
                }
                std::cmp::Ordering::Greater => matched_t.get((k - 1) as usize).copied(),
            }
        }
    }
}

impl TokenClass {
    fn matched(&self, c: &char) -> bool {
        match self {
            TokenClass::NumTok => '0' <= *c && *c <= '9',
            TokenClass::AlphaTok => c.is_alphabetic(),
            TokenClass::UpperTok => c.is_ascii_uppercase(),
            TokenClass::LowerTok => c.is_ascii_lowercase(),
            TokenClass::WhileSpaceTok => c.is_whitespace(),
        }
    }
}

impl Special {
    fn matched(&self, c: &char) -> bool {
        match self {
            Special::Is(cc) => cc == c,
            Special::Not(cc) => cc != c,
            // if we do not handle these token early, must be unmatch
            Special::Start | Special::End => false,
        }
    }
}

impl Index<FreeString> for InputState {
    type Output = String;
    fn index(&self, v: FreeString) -> &String {
        &self.0[v]
    }
}

/// Test cases below are taken from the paper examples
#[cfg(test)]
mod tests {
    use super::*;
    use crate::examples::*;

    fn split(example: &[&str]) -> (InputState, String) {
        let input = example
            .iter()
            .take(example.len() - 1)
            .map(|s| s.to_string())
            .collect();
        let expected = example[example.len() - 1].to_string();
        (InputState(input), expected)
    }

    #[test]
    fn test_regular() {
        let regular = RegularExpr::empty();
        assert!(eval_regular(&regular, ""));
        let regular = RegularExpr(vec![Token::OneOrMore(TokenClass::NumTok)]);
        assert!(eval_regular(&regular, "1"));
        assert!(eval_regular(&regular, "123"));
        assert!(!eval_regular(&regular, "123a"));
        let regular = RegularExpr(vec![Token::Special('/'.into())]);
        assert!(eval_regular(&regular, "/"));
        assert!(!eval_regular(&regular, "1/123"));
    }

    /// Example 2: extract the quantity of the purchase
    #[test]
    fn test2() {
        let p2 = AtomicExpr::SubStr {
            v: 0,
            p1: Position::Pos {
                // match epsilon, empty string
                r1: RegularExpr::empty(),
                r2: Token::OneOrMore(TokenClass::NumTok).into(),
                c: IntegerExpr::Constant(1),
            },
            p2: Position::CPos(-1),
        };
        for s in EXAMPLE2.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval_atomic(&p2, &sigma), Some(expected));
        }
    }

    /// Example 3: extract directory name from a path
    #[test]
    fn test3() {
        let p3 = AtomicExpr::SubStr {
            v: 0,
            p1: Position::CPos(0),
            p2: Position::Pos {
                r1: Token::Special('/'.into()).into(),
                r2: RegularExpr::empty(),
                c: IntegerExpr::Constant(-1),
            },
        };
        for s in EXAMPLE3.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval_atomic(&p3, &sigma), Some(expected));
        }
    }

    /// Example 4: generate abbreviations of a string
    #[test]
    fn test4() {
        let p4 = AtomicExpr::Loop {
            e: sub_str2(
                0,
                Token::OneOrMore(TokenClass::UpperTok).into(),
                IntegerExpr::Bound,
            )
            .into(),
        };
        for s in EXAMPLE4.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval_atomic(&p4, &sigma), Some(expected));
        }
    }

    /// Example 5: split odds
    #[test]
    fn test5() {
        let pos1 = Position::Pos {
            r1: Token::Special('('.into()).into(),
            r2: vec![
                Token::OneOrMore(TokenClass::NumTok),
                Token::Special('/'.into()),
            ]
            .into(),
            c: IntegerExpr::Bound,
        };
        let pos2 = Position::Pos {
            r1: vec![
                Token::Special('/'.into()),
                Token::OneOrMore(TokenClass::NumTok),
            ]
            .into(),
            r2: Token::Special(')'.into()).into(),
            c: IntegerExpr::Bound,
        };
        let p5 = AtomicExpr::Loop {
            e: TraceExpr(vec![
                AtomicExpr::SubStr {
                    v: 0,
                    p1: pos1,
                    p2: pos2,
                },
                AtomicExpr::ConstStr("# ".to_string()),
            ]),
        }
        .into();
        for s in EXAMPLE5.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval_trace(&p5, &sigma), Some(expected));
        }
    }

    /// Example 6: remove excess spaces
    #[test]
    fn test6() {
        let pos1 = Position::Pos {
            r1: RegularExpr::empty(),
            r2: Token::ExcludeOneOrMore(TokenClass::WhileSpaceTok).into(),
            c: IntegerExpr::Bound,
        };
        let pos2 = Position::Pos {
            r1: Token::ExcludeOneOrMore(TokenClass::WhileSpaceTok).into(),
            r2: vec![
                Token::OneOrMore(TokenClass::WhileSpaceTok),
                Token::ExcludeOneOrMore(TokenClass::WhileSpaceTok),
            ]
            .into(),
            c: IntegerExpr::Bound,
        };
        let p6 = vec![
            AtomicExpr::Loop {
                e: TraceExpr(vec![
                    AtomicExpr::SubStr {
                        v: 0,
                        p1: pos1,
                        p2: pos2,
                    },
                    AtomicExpr::ConstStr(" ".to_string()),
                ]),
            },
            sub_str2(
                0,
                Token::ExcludeOneOrMore(TokenClass::WhileSpaceTok).into(),
                IntegerExpr::Constant(-1),
            ),
        ]
        .into();
        for s in EXAMPLE6.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval_trace(&p6, &sigma), Some(expected));
        }
    }

    /// Example 7: conditional concatenation
    #[test]
    fn test7() {
        let b1: Bool = Conjunct(vec![
            Predicate::Is(Match::new(
                0,
                Token::OneOrMore(TokenClass::AlphaTok).into(),
                1,
            )),
            Predicate::Is(Match::new(
                1,
                Token::OneOrMore(TokenClass::AlphaTok).into(),
                1,
            )),
        ])
        .into();
        let e1 = vec![
            AtomicExpr::SubStr {
                v: 0,
                p1: Position::CPos(0),
                p2: Position::CPos(-1),
            },
            AtomicExpr::ConstStr("(".to_string()),
            AtomicExpr::SubStr {
                v: 1,
                p1: Position::CPos(0),
                p2: Position::CPos(-1),
            },
            AtomicExpr::ConstStr(")".to_string()),
        ]
        .into();
        let b2 = Bool(vec![
            Conjunct(vec![Predicate::Is(Match::new(
                0,
                Token::OneOrMore(TokenClass::AlphaTok).into(),
                1,
            ))]),
            Conjunct(vec![Predicate::Is(Match::new(
                1,
                Token::OneOrMore(TokenClass::AlphaTok).into(),
                1,
            ))]),
        ]);
        let p7 = vec![(b1, e1), (b2, AtomicExpr::ConstStr("".to_string()).into())].into();
        for s in EXAMPLE7.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval(&p7, &sigma), Some(expected));
        }
    }

    /// Example 8: mixed date parsing
    #[test]
    fn test8() {
        let b1: Bool = Conjunct(vec![Predicate::Is(Match::new(
            0,
            Token::Special('/'.into()).into(),
            1,
        ))])
        .into();
        let b2: Bool = Conjunct(vec![Predicate::Is(Match::new(
            0,
            Token::Special('.'.into()).into(),
            1,
        ))])
        .into();
        let b3: Bool = Conjunct(vec![Predicate::Is(Match::new(
            0,
            Token::Special('-'.into()).into(),
            1,
        ))])
        .into();
        let e1 = AtomicExpr::SubStr {
            v: 0,
            p1: Position::Pos {
                r1: Token::Special(Special::Start).into(),
                r2: RegularExpr::empty(),
                c: IntegerExpr::Constant(1),
            },
            p2: Position::Pos {
                r1: RegularExpr::empty(),
                r2: Token::Special('/'.into()).into(),
                c: IntegerExpr::Constant(1),
            },
        };
        let e2 = AtomicExpr::SubStr {
            v: 0,
            p1: Position::Pos {
                r1: Token::Special('.'.into()).into(),
                r2: RegularExpr::empty(),
                c: IntegerExpr::Constant(1),
            },
            p2: Position::Pos {
                r1: RegularExpr::empty(),
                r2: Token::Special('.'.into()).into(),
                c: IntegerExpr::Constant(2),
            },
        };
        let e3 = AtomicExpr::SubStr {
            v: 0,
            p1: Position::Pos {
                r1: Token::Special('-'.into()).into(),
                r2: RegularExpr::empty(),
                c: IntegerExpr::Constant(2),
            },
            p2: Position::Pos {
                r1: Token::Special(Special::End).into(),
                r2: RegularExpr::empty(),
                c: IntegerExpr::Constant(1),
            },
        };
        let p8 = vec![(b1, e1.into()), (b2, e2.into()), (b3, e3.into())].into();
        for s in EXAMPLE8.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval(&p8, &sigma), Some(expected));
        }
    }

    /// Example 9: name parsing
    #[test]
    fn test9() {
        let p1 = Position::Pos {
            r1: RegularExpr::empty(),
            r2: vec![
                Token::OneOrMore(TokenClass::AlphaTok),
                Token::Special(Special::Not('.')),
            ]
            .into(),
            c: IntegerExpr::Constant(1),
        };
        let p2 = Position::Pos {
            r1: RegularExpr::empty(),
            r2: vec![
                Token::OneOrMore(TokenClass::LowerTok),
                Token::Special(Special::Not('.')),
            ]
            .into(),
            c: IntegerExpr::Constant(1),
        };
        let last_name = AtomicExpr::SubStr { v: 0, p1, p2 };
        let p1 = Position::Pos {
            r1: RegularExpr::empty(),
            r2: vec![
                Token::OneOrMore(TokenClass::AlphaTok),
                Token::Special(Special::Is(',')),
            ]
            .into(),
            c: IntegerExpr::Constant(1),
        };
        let p2 = Position::Pos {
            r1: Token::OneOrMore(TokenClass::AlphaTok).into(),
            r2: Token::Special(Special::Is(',')).into(),
            c: IntegerExpr::Constant(1),
        };
        let b1: Bool = Conjunct(vec![Predicate::Is(Match::new(
            0,
            Token::Special(','.into()).into(),
            1,
        ))])
        .into();
        let e1 = vec![
            AtomicExpr::SubStr { v: 0, p1, p2 },
            AtomicExpr::ConstStr(", ".to_string()),
            last_name.clone(),
            AtomicExpr::ConstStr(".".to_string()),
        ]
        .into();
        let e2 = vec![
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::AlphaTok).into(),
                IntegerExpr::Constant(-1),
            ),
            AtomicExpr::ConstStr(", ".to_string()),
            last_name.clone(),
            AtomicExpr::ConstStr(".".to_string()),
        ]
        .into();
        let b2: Bool = Conjunct(vec![Predicate::Not(Match::new(
            0,
            Token::Special(','.into()).into(),
            1,
        ))])
        .into();
        let p13 = vec![(b1, e1), (b2, e2)].into();
        for s in EXAMPLE9.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval(&p13, &sigma), Some(expected));
        }
    }

    /// Example 10: phone number
    #[test]
    fn test10() {
        let b1: Bool = Conjunct(vec![Predicate::Is(Match::new(
            0,
            Token::OneOrMore(TokenClass::NumTok).into(),
            3,
        ))])
        .into();
        let e1 = vec![
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::NumTok).into(),
                IntegerExpr::Constant(1),
            ),
            AtomicExpr::ConstStr("-".to_string()),
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::NumTok).into(),
                IntegerExpr::Constant(2),
            ),
            AtomicExpr::ConstStr("-".to_string()),
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::NumTok).into(),
                IntegerExpr::Constant(3),
            ),
        ]
        .into();
        let b2: Bool = Conjunct(vec![Predicate::Not(Match::new(
            0,
            Token::OneOrMore(TokenClass::NumTok).into(),
            3,
        ))])
        .into();
        let e2 = vec![
            AtomicExpr::ConstStr("425-".to_string()),
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::NumTok).into(),
                IntegerExpr::Constant(1),
            ),
            AtomicExpr::ConstStr("-".to_string()),
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::NumTok).into(),
                IntegerExpr::Constant(2),
            ),
        ]
        .into();
        let p10 = vec![(b1, e1), (b2, e2)].into();
        for s in EXAMPLE10.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval(&p10, &sigma), Some(expected));
        }
    }

    /// Example 12: Synthesis of part of a futre extension of itself
    #[test]
    fn test12() {
        let p12 = vec![
            AtomicExpr::ConstStr("case ".to_string()),
            AtomicExpr::SubStr {
                v: 1,
                p1: Position::CPos(0),
                p2: Position::CPos(-1),
            },
            AtomicExpr::ConstStr(": return \"".to_string()),
            AtomicExpr::SubStr {
                v: 0,
                p1: Position::CPos(0),
                p2: Position::CPos(-1),
            },
            AtomicExpr::ConstStr("\";".to_string()),
        ]
        .into();
        for s in EXAMPLE12.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval_trace(&p12, &sigma), Some(expected));
        }
    }

    /// Example 13: filtering task
    #[test]
    fn test13() {
        let b1: Bool = Conjunct(vec![Predicate::Is(Match::new(
            0,
            Token::Special('/'.into()).into(),
            6,
        ))])
        .into();
        let e1 = vec![AtomicExpr::SubStr {
            v: 1,
            p1: Position::CPos(0),
            p2: Position::CPos(-1),
        }]
        .into();
        let b2: Bool = Conjunct(vec![Predicate::Not(Match::new(
            0,
            Token::Special('/'.into()).into(),
            6,
        ))])
        .into();
        let p13 = vec![(b1, e1), (b2, AtomicExpr::ConstStr("0".to_string()).into())].into();
        for s in EXAMPLE13.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval(&p13, &sigma), Some(expected));
        }
    }

    /// Example 14: arithmetic
    #[test]
    fn test14() {
        let pos1 = Position::Pos {
            r1: RegularExpr::empty(),
            r2: Token::OneOrMore(TokenClass::NumTok).into(),
            c: IntegerExpr::Bound,
        };
        let pos2 = Position::Pos {
            r1: Token::OneOrMore(TokenClass::NumTok).into(),
            r2: vec![
                Token::ExcludeOneOrMore(TokenClass::NumTok),
                Token::OneOrMore(TokenClass::NumTok),
            ]
            .into(),
            c: IntegerExpr::Bound,
        };
        let p14 = vec![
            AtomicExpr::Loop {
                e: TraceExpr(vec![
                    AtomicExpr::SubStr {
                        v: 0,
                        p1: pos1,
                        p2: pos2,
                    },
                    AtomicExpr::ConstStr("+".to_string()),
                ]),
            },
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::NumTok).into(),
                IntegerExpr::Constant(-1),
            ),
        ]
        .into();
        println!("{}", p14);
        for s in EXAMPLE14.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(eval_trace(&p14, &sigma), Some(expected));
        }
    }
}
