//! Syntax of String Expressions P
//!
//! Corresponds to the Figure 1 of the paper

use std::fmt::{self, Display, Formatter};

use itertools::Itertools;
use lazy_static::lazy_static;

lazy_static! {
    static ref ALLTOKENS: Vec<Token> = {
        let specials = ['/', '-', '(', ')', '.', ','];
        let _ = specials.iter().map(|c| Token::Special(Special::Is(*c)));
        let classes = [
            TokenClass::Alpha,
            TokenClass::Lower,
            TokenClass::Upper,
            TokenClass::Num,
            TokenClass::Space,
        ];
        let mut tokens = vec![];
        for s in specials {
            tokens.push(Token::Special(Special::Is(s)));
        }
        for c in classes {
            tokens.push(Token::OneOrMore(c));
            tokens.push(Token::ExcludeOneOrMore(c));
        }
        tokens
    };
    // all tokenseqs are limited now
    pub static ref ALLTOKENSEQS: Vec<RegularExpr> = {
        let mut all = ALLTOKENS
        .iter()
        .map(|tokens| RegularExpr(vec![*tokens]))
        .collect_vec();
        all.insert(0, RegularExpr::empty());
        all
    };
}

/// String expr P := Switch((b1, e1), ..., (bn, en))
#[derive(Debug, Clone)]
pub struct StringExpr(pub Vec<(Bool, TraceExpr)>);

/// Bool expr b := d1 & ... & dn
#[derive(Debug, Clone)]
pub struct Bool(pub Vec<Conjunct>);

/// Conjunct c := p1 | ... | pn
#[derive(Debug, Clone)]
pub struct Conjunct(pub Vec<Predicate>);

/// Predicate p := Match | !Match
#[derive(Debug, Clone)]
pub enum Predicate {
    Is(Match),
    Not(Match),
}

/// Match m := (v, r, k)
///
/// true iff vi contains at least k occurrences of r
#[derive(Debug, Clone)]
pub struct Match {
    pub v: FreeString,
    pub r: RegularExpr,
    pub k: usize,
}

/// Trace expr e := Concat(f1, ..., fn)
#[derive(Debug, Clone)]
pub struct TraceExpr(pub Vec<AtomicExpr>);

/// Atomic expr f := SubStr(v, p1, p2) | ConstStr(s) | Loop(\lambda w: e)
#[derive(Debug, Clone)]
pub enum AtomicExpr {
    /// SubStr(v, p1, p2) constructs a substring of v from position p1 to p2
    SubStr {
        v: FreeString,
        p1: Position,
        p2: Position,
    },
    ConstStr(String),
    /// Loop(\lambda w: e) constructs a string by looping over e
    ///
    /// where w is a bound integer variable
    Loop {
        e: TraceExpr,
    },
}

#[derive(Debug, Clone)]
pub enum Position {
    /// CPos(k) is the k-th character of the string
    CPos(i32),
    /// Pos(r1, r2, c) evaluates an index t s.t. r1 matches the s\[0..t-1\] and r2 matches the s\[t..n-1\]
    Pos {
        r1: RegularExpr,
        r2: RegularExpr,
        c: IntegerExpr,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntegerExpr {
    Constant(i32),
    /// simple bound now
    Bound,
}

#[derive(Debug, Clone)]
pub struct RegularExpr(pub Vec<Token>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    Special(Special),
    OneOrMore(TokenClass),
    ExcludeOneOrMore(TokenClass),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Special {
    Is(char),
    Not(char),
    Start,
    End,
}

/// classical token class
///
/// e.g. NumTok, AlphaTok
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenClass {
    Num,
    Alpha,
    Upper,
    Lower,
    Space,
}

pub type FreeString = usize;

impl RegularExpr {
    pub fn empty() -> Self {
        Self(vec![])
    }

    pub fn concat(&self, other: &Self) -> Self {
        Self(itertools::concat([self.0.clone(), other.0.clone()]))
    }
}

impl Match {
    pub fn new(v: FreeString, r: RegularExpr, k: usize) -> Self {
        Self { v, r, k }
    }
}

impl From<Conjunct> for Bool {
    fn from(conj: Conjunct) -> Self {
        Bool(vec![conj])
    }
}

impl From<AtomicExpr> for TraceExpr {
    fn from(value: AtomicExpr) -> Self {
        Self(vec![value])
    }
}

impl From<Token> for RegularExpr {
    fn from(value: Token) -> Self {
        Self(vec![value])
    }
}

impl From<char> for Special {
    fn from(value: char) -> Self {
        Self::Is(value)
    }
}

impl Display for StringExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.0.len() == 1 {
            write!(f, "{}", self.0[0].1)
        } else {
            writeln!(f, "Switch(")?;
            for (b, e) in self.0.iter() {
                writeln!(f, "{}, {}", b, e)?;
            }
            write!(f, ")")
        }
    }
}

impl Display for Bool {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "(")?;
        for (i, c) in self.0.iter().enumerate() {
            write!(f, "{}", c)?;
            if i != self.0.len() - 1 {
                write!(f, " & ")?;
            }
        }
        write!(f, ")")
    }
}

impl Display for Conjunct {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "(")?;
        for (i, p) in self.0.iter().enumerate() {
            write!(f, "{}", p)?;
            if i != self.0.len() - 1 {
                write!(f, " | ")?;
            }
        }
        write!(f, ")")
    }
}

impl Display for Predicate {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Predicate::Is(m) => write!(f, "{}", m),
            Predicate::Not(m) => write!(f, "!{}", m),
        }
    }
}

impl Display for Match {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "(v{}, {}, {})", self.v + 1, self.r, self.k)
    }
}

impl Display for TraceExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Concat(")?;
        for (i, e) in self.0.iter().enumerate() {
            write!(f, "{}", e)?;
            if i != self.0.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl Display for AtomicExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            AtomicExpr::SubStr { v, p1, p2 } => write!(f, "SubStr({}, {}, {})", v, p1, p2),
            AtomicExpr::ConstStr(s) => write!(f, "ConstStr({})", s),
            AtomicExpr::Loop { e } => write!(f, "Loop(lambda w: {})", e),
        }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Position::CPos(k) => write!(f, "CPos({})", k),
            Position::Pos { r1, r2, c } => write!(f, "Pos({}, {}, {})", r1, r2, c),
        }
    }
}

impl Display for IntegerExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            IntegerExpr::Constant(k) => write!(f, "{}", k),
            IntegerExpr::Bound => write!(f, "w"),
        }
    }
}

impl Display for RegularExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.0.is_empty() {
            write!(f, "e")?;
        }
        for (i, t) in self.0.iter().enumerate() {
            write!(f, "{}", t)?;
            if i != self.0.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::Special(s) => write!(f, "{}", s),
            Token::OneOrMore(tc) => write!(f, "{}+", tc),
            Token::ExcludeOneOrMore(tc) => write!(f, "Not({}+)", tc),
        }
    }
}

impl Display for Special {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Special::Is(c) => write!(f, "'{c}'"),
            Special::Not(c) => write!(f, "Not('{c}')"),
            Special::Start => write!(f, "start"),
            Special::End => write!(f, "end"),
        }
    }
}

impl Display for TokenClass {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TokenClass::Num => write!(f, "NumTok"),
            TokenClass::Alpha => write!(f, "AlphaTok"),
            TokenClass::Upper => write!(f, "UpperTok"),
            TokenClass::Lower => write!(f, "LowerTok"),
            TokenClass::Space => write!(f, "Space"),
        }
    }
}
