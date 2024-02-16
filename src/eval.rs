//! Semantics of String Expressions P
//!
//! Corresponds to the Figure 2 of the paper

use crate::language::*;
use crate::util::InputState;

pub trait Eval<T = String> {
    fn eval(&self, sigma: &InputState) -> Option<T>;
}
pub trait EvalBool<T = InputState> {
    fn eval(&self, sigma: T) -> bool;
}

trait SubStitute {
    fn substitute(&self, k: usize) -> Self;
}

impl Eval for StringExpr {
    /// Option is used to represent the undefined value ⊥
    fn eval(&self, sigma: &InputState) -> Option<String> {
        for (b, e) in &self.0 {
            if b.eval(sigma) {
                return e.eval(sigma);
            }
        }
        None
    }
}

impl EvalBool<&InputState> for Bool {
    fn eval(&self, sigma: &InputState) -> bool {
        for c in &self.0 {
            if c.eval(sigma) {
                return true;
            }
        }
        false
    }
}

impl EvalBool<&InputState> for Conjunct {
    fn eval(&self, sigma: &InputState) -> bool {
        for p in &self.0 {
            if !p.eval(sigma) {
                return false;
            }
        }
        true
    }
}

impl EvalBool<&InputState> for Predicate {
    fn eval(&self, sigma: &InputState) -> bool {
        match self {
            Predicate::Is(m) => m.eval(sigma),
            Predicate::Not(m) => !m.eval(sigma),
        }
    }
}

impl EvalBool<&InputState> for Match {
    fn eval(&self, sigma: &InputState) -> bool {
        let s = &sigma[self.v];
        let r = &self.r;
        r.find_all_matches(s).collect::<Vec<_>>().len() >= self.k
    }
}

impl EvalBool<&str> for RegularExpr {
    fn eval(&self, s: &str) -> bool {
        // empty sequence + empty string
        if s.is_empty() {
            return self.0.is_empty();
        }
        let mut s = s.chars().peekable();
        for token in &self.0 {
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
}

impl Eval for TraceExpr {
    fn eval(&self, sigma: &InputState) -> Option<String> {
        self.0.iter().try_fold("".to_string(), |acc, f| {
            let s1 = f.eval(sigma)?;
            Some(acc + &s1)
        })
    }
}

impl Eval for AtomicExpr {
    fn eval(&self, sigma: &InputState) -> Option<String> {
        match self {
            AtomicExpr::SubStr { v, p1, p2 } => {
                let p1 = eval(p1, &sigma[*v])?;
                let p2 = eval(p2, &sigma[*v])?;
                // Paper use [p1..p2] and [p1..=p2] mix!
                Some(sigma[*v][p1..p2].to_string())
            }
            AtomicExpr::ConstStr(s) => Some(s.clone()),
            AtomicExpr::Loop { e } => Some(eval_loop(e, 1, sigma)),
        }
    }
}

impl SubStitute for TraceExpr {
    fn substitute(&self, k: usize) -> TraceExpr {
        TraceExpr(self.0.iter().map(|f| (f.substitute(k))).collect())
    }
}

impl SubStitute for AtomicExpr {
    fn substitute(&self, k: usize) -> AtomicExpr {
        match self {
            AtomicExpr::SubStr { v, p1, p2 } => AtomicExpr::SubStr {
                v: *v,
                p1: p1.substitute(k),
                p2: p2.substitute(k),
            },
            AtomicExpr::ConstStr(_) => self.clone(),
            AtomicExpr::Loop { e: _ } => unreachable!("loop twice"),
        }
    }
}

impl SubStitute for Position {
    fn substitute(&self, k: usize) -> Position {
        match self {
            Position::CPos(_) => self.clone(),
            Position::Pos { r1, r2, c } => {
                let c = match c {
                    IntegerExpr::Constant(_) => *c,
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
}

fn eval_loop(e: &TraceExpr, k: usize, sigma: &InputState) -> String {
    let t = e.substitute(k);
    match t.eval(sigma) {
        Some(s) => s + eval_loop(e, k + 1, sigma).as_str(),
        None => "".to_string(),
    }
}

fn eval(p: &Position, s: &str) -> Option<usize> {
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
            for (_t1, t3) in r1.find_all_matches(s) {
                for (t, _t2) in r2.find_all_matches(s) {
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

impl RegularExpr {
    fn find_all_matches(&self, s: &str) -> impl Iterator<Item = (isize, isize)> {
        if self.0.contains(&Token::Special(Special::Start)) {
            let mut r = self.clone();
            r.0.remove(0);
            if r.0.is_empty() {
                return vec![(0, -1)].into_iter();
            }
            unimplemented!("not only single startTok")
        } else if self.0.contains(&Token::Special(Special::End)) {
            let mut r = self.clone();
            r.0.remove(r.0.len() - 1);
            if r.0.is_empty() {
                return vec![(s.len() as isize, s.len() as isize - 1)].into_iter();
            }
            unimplemented!("not only single endTok");
        } else {
            if self.0.is_empty() {
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
                    if self.eval(&s[start..=end]) {
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
}

impl TokenClass {
    fn matched(&self, c: &char) -> bool {
        match self {
            TokenClass::Num => '0' <= *c && *c <= '9',
            TokenClass::Alpha => c.is_alphabetic(),
            TokenClass::Upper => c.is_ascii_uppercase(),
            TokenClass::Lower => c.is_ascii_lowercase(),
            TokenClass::WhileSpace => c.is_whitespace(),
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

/// Test cases below are taken from the paper examples
#[cfg(test)]
mod tests {
    use super::*;
    use crate::examples::*;
    use crate::util::split;

    fn sub_str2(v: FreeString, r: RegularExpr, c: IntegerExpr) -> AtomicExpr {
        AtomicExpr::SubStr {
            v,
            p1: Position::Pos {
                r1: RegularExpr::empty(),
                r2: r.clone(),
                c,
            },
            p2: Position::Pos {
                r1: r,
                r2: RegularExpr::empty(),
                c,
            },
        }
    }

    /// Example 2: extract the quantity of the purchase
    #[test]
    fn test2() {
        let p2 = AtomicExpr::SubStr {
            v: 0,
            p1: Position::Pos {
                // match epsilon, empty string
                r1: RegularExpr::empty(),
                r2: Token::OneOrMore(TokenClass::Num).into(),
                c: IntegerExpr::Constant(1),
            },
            p2: Position::CPos(-1),
        };
        for s in EXAMPLE2.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p2.eval(&sigma), Some(expected));
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
            assert_eq!(p3.eval(&sigma), Some(expected));
        }
    }

    /// Example 4: generate abbreviations of a string
    #[test]
    fn test4() {
        let p4 = AtomicExpr::Loop {
            e: sub_str2(
                0,
                Token::OneOrMore(TokenClass::Upper).into(),
                IntegerExpr::Bound,
            )
            .into(),
        };
        for s in EXAMPLE4.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p4.eval(&sigma), Some(expected));
        }
    }

    /// Example 5: split odds
    #[test]
    fn test5() {
        let pos1 = Position::Pos {
            r1: Token::Special('('.into()).into(),
            r2: RegularExpr(vec![
                Token::OneOrMore(TokenClass::Num),
                Token::Special('/'.into()),
            ]),
            c: IntegerExpr::Bound,
        };
        let pos2 = Position::Pos {
            r1: RegularExpr(vec![
                Token::Special('/'.into()),
                Token::OneOrMore(TokenClass::Num),
            ]),
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
        };
        for s in EXAMPLE5.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p5.eval(&sigma), Some(expected));
        }
    }

    /// Example 6: remove excess spaces
    #[test]
    fn test6() {
        let pos1 = Position::Pos {
            r1: RegularExpr::empty(),
            r2: Token::ExcludeOneOrMore(TokenClass::WhileSpace).into(),
            c: IntegerExpr::Bound,
        };
        let pos2 = Position::Pos {
            r1: Token::ExcludeOneOrMore(TokenClass::WhileSpace).into(),
            r2: RegularExpr(vec![
                Token::OneOrMore(TokenClass::WhileSpace),
                Token::ExcludeOneOrMore(TokenClass::WhileSpace),
            ]),
            c: IntegerExpr::Bound,
        };
        let p6 = TraceExpr(vec![
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
                Token::ExcludeOneOrMore(TokenClass::WhileSpace).into(),
                IntegerExpr::Constant(-1),
            ),
        ]);
        for s in EXAMPLE6.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p6.eval(&sigma), Some(expected));
        }
    }

    /// Example 7: conditional concatenation
    #[test]
    fn test7() {
        let b1: Bool = Conjunct(vec![
            Predicate::Is(Match::new(0, Token::OneOrMore(TokenClass::Alpha).into(), 1)),
            Predicate::Is(Match::new(1, Token::OneOrMore(TokenClass::Alpha).into(), 1)),
        ])
        .into();
        let e1 = TraceExpr(vec![
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
        ]);
        let b2 = Bool(vec![
            Conjunct(vec![Predicate::Is(Match::new(
                0,
                Token::OneOrMore(TokenClass::Alpha).into(),
                1,
            ))]),
            Conjunct(vec![Predicate::Is(Match::new(
                1,
                Token::OneOrMore(TokenClass::Alpha).into(),
                1,
            ))]),
        ]);
        let p7 = StringExpr(vec![
            (b1, e1),
            (b2, AtomicExpr::ConstStr("".to_string()).into()),
        ]);
        for s in EXAMPLE7.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p7.eval(&sigma), Some(expected));
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
        let p8 = StringExpr(vec![(b1, e1.into()), (b2, e2.into()), (b3, e3.into())]);
        for s in EXAMPLE8.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p8.eval(&sigma), Some(expected));
        }
    }

    /// Example 9: name parsing
    #[test]
    fn test9() {
        let p1 = Position::Pos {
            r1: RegularExpr::empty(),
            r2: RegularExpr(vec![
                Token::OneOrMore(TokenClass::Alpha),
                Token::Special(Special::Not('.')),
            ]),
            c: IntegerExpr::Constant(1),
        };
        let p2 = Position::Pos {
            r1: RegularExpr::empty(),
            r2: RegularExpr(vec![
                Token::OneOrMore(TokenClass::Lower),
                Token::Special(Special::Not('.')),
            ]),
            c: IntegerExpr::Constant(1),
        };
        let last_name = AtomicExpr::SubStr { v: 0, p1, p2 };
        let p1 = Position::Pos {
            r1: RegularExpr::empty(),
            r2: RegularExpr(vec![
                Token::OneOrMore(TokenClass::Alpha),
                Token::Special(Special::Is(',')),
            ]),
            c: IntegerExpr::Constant(1),
        };
        let p2 = Position::Pos {
            r1: Token::OneOrMore(TokenClass::Alpha).into(),
            r2: Token::Special(Special::Is(',')).into(),
            c: IntegerExpr::Constant(1),
        };
        let b1: Bool = Conjunct(vec![Predicate::Is(Match::new(
            0,
            Token::Special(','.into()).into(),
            1,
        ))])
        .into();
        let e1 = TraceExpr(vec![
            AtomicExpr::SubStr { v: 0, p1, p2 },
            AtomicExpr::ConstStr(", ".to_string()),
            last_name.clone(),
            AtomicExpr::ConstStr(".".to_string()),
        ]);
        let e2 = TraceExpr(vec![
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::Alpha).into(),
                IntegerExpr::Constant(-1),
            ),
            AtomicExpr::ConstStr(", ".to_string()),
            last_name.clone(),
            AtomicExpr::ConstStr(".".to_string()),
        ]);
        let b2: Bool = Conjunct(vec![Predicate::Not(Match::new(
            0,
            Token::Special(','.into()).into(),
            1,
        ))])
        .into();
        let p13 = StringExpr(vec![(b1, e1), (b2, e2)]);
        for s in EXAMPLE9.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p13.eval(&sigma), Some(expected));
        }
    }

    /// Example 10: phone number
    #[test]
    fn test10() {
        let b1: Bool = Conjunct(vec![Predicate::Is(Match::new(
            0,
            Token::OneOrMore(TokenClass::Num).into(),
            3,
        ))])
        .into();
        let e1 = TraceExpr(vec![
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::Num).into(),
                IntegerExpr::Constant(1),
            ),
            AtomicExpr::ConstStr("-".to_string()),
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::Num).into(),
                IntegerExpr::Constant(2),
            ),
            AtomicExpr::ConstStr("-".to_string()),
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::Num).into(),
                IntegerExpr::Constant(3),
            ),
        ]);
        let b2: Bool = Conjunct(vec![Predicate::Not(Match::new(
            0,
            Token::OneOrMore(TokenClass::Num).into(),
            3,
        ))])
        .into();
        let e2 = TraceExpr(vec![
            AtomicExpr::ConstStr("425-".to_string()),
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::Num).into(),
                IntegerExpr::Constant(1),
            ),
            AtomicExpr::ConstStr("-".to_string()),
            sub_str2(
                0,
                Token::OneOrMore(TokenClass::Num).into(),
                IntegerExpr::Constant(2),
            ),
        ]);
        let p10 = StringExpr(vec![(b1, e1), (b2, e2)]);
        for s in EXAMPLE10.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p10.eval(&sigma), Some(expected));
        }
    }

    /// Example 12: Synthesis of part of a futre extension of itself
    #[test]
    fn test12() {
        let p12 = TraceExpr(vec![
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
        ]);
        for s in EXAMPLE12.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p12.eval(&sigma), Some(expected));
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
        let e1 = TraceExpr(vec![AtomicExpr::SubStr {
            v: 1,
            p1: Position::CPos(0),
            p2: Position::CPos(-1),
        }]);
        let b2: Bool = Conjunct(vec![Predicate::Not(Match::new(
            0,
            Token::Special('/'.into()).into(),
            6,
        ))])
        .into();
        let p13 = StringExpr(vec![
            (b1, e1),
            (b2, AtomicExpr::ConstStr("0".to_string()).into()),
        ]);
        for s in EXAMPLE13.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p13.eval(&sigma), Some(expected));
        }
    }

    /// Example 14: arithmetic
    #[test]
    fn test14() {
        let pos1 = Position::Pos {
            r1: RegularExpr::empty(),
            r2: Token::OneOrMore(TokenClass::Num).into(),
            c: IntegerExpr::Bound,
        };
        let pos2 = Position::Pos {
            r1: Token::OneOrMore(TokenClass::Num).into(),
            r2: RegularExpr(vec![
                Token::ExcludeOneOrMore(TokenClass::Num),
                Token::OneOrMore(TokenClass::Num),
            ]),
            c: IntegerExpr::Bound,
        };
        let p14 = TraceExpr(vec![
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
                Token::OneOrMore(TokenClass::Num).into(),
                IntegerExpr::Constant(-1),
            ),
        ]);
        println!("{}", p14);
        for s in EXAMPLE14.iter() {
            let (sigma, expected) = split(s);
            assert_eq!(p14.eval(&sigma), Some(expected));
        }
    }
}
