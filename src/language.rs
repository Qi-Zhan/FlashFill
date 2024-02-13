//! Syntax of String Expressions P
//!
//! Corresponds to the Figure 1 of the paper

use std::fmt::{self, Display, Formatter};
use std::ops::{Deref, Index};

/// String expr P := Switch((b1, e1), ..., (bn, en))
#[derive(Debug, Clone)]
pub struct StringExpr {
    pub switches: Vec<(Bool, TraceExpr)>,
}

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

pub fn sub_str2(v: FreeString, r: RegularExpr, c: IntegerExpr) -> AtomicExpr {
    AtomicExpr::SubStr {
        v,
        p1: Position::Pos {
            r1: RegularExpr::empty(),
            r2: r.clone(),
            c: c.clone(),
        },
        p2: Position::Pos {
            r1: r,
            r2: RegularExpr::empty(),
            c,
        },
    }
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

#[derive(Debug, Clone)]
pub enum IntegerExpr {
    Constant(i32),
    /// simple bound now
    Bound,
}

#[derive(Debug, Clone)]
pub struct RegularExpr(pub Vec<Token>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Special(Special),
    OneOrMore(TokenClass),
    ExcludeOneOrMore(TokenClass),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Special {
    Is(char),
    Not(char),
    Start,
    End,
}

/// classical token class
///
/// e.g. NumTok, AlphaTok
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenClass {
    NumTok,
    AlphaTok,
    UpperTok,
    LowerTok,
    WhileSpaceTok,
}

pub type FreeString = usize;

impl RegularExpr {
    pub fn empty() -> Self {
        Self(vec![])
    }
}

impl Match {
    pub fn new(v: FreeString, r: RegularExpr, k: usize) -> Self {
        Self { v, r, k }
    }
}

impl Index<usize> for StringExpr {
    type Output = (Bool, TraceExpr);
    fn index(&self, i: usize) -> &(Bool, TraceExpr) {
        &self.switches[i]
    }
}

impl Index<usize> for Bool {
    type Output = Conjunct;
    fn index(&self, i: usize) -> &Conjunct {
        &self.0[i]
    }
}

impl Index<usize> for Conjunct {
    type Output = Predicate;
    fn index(&self, i: usize) -> &Predicate {
        &self.0[i]
    }
}

impl Index<usize> for TraceExpr {
    type Output = AtomicExpr;
    fn index(&self, i: usize) -> &AtomicExpr {
        &self.0[i]
    }
}

impl Index<usize> for RegularExpr {
    type Output = Token;
    fn index(&self, i: usize) -> &Token {
        &self.0[i]
    }
}

impl Deref for StringExpr {
    type Target = Vec<(Bool, TraceExpr)>;
    fn deref(&self) -> &Self::Target {
        &self.switches
    }
}

impl Deref for Bool {
    type Target = Vec<Conjunct>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Deref for Conjunct {
    type Target = Vec<Predicate>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Deref for TraceExpr {
    type Target = Vec<AtomicExpr>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Deref for RegularExpr {
    type Target = Vec<Token>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Vec<(Bool, TraceExpr)>> for StringExpr {
    fn from(sw: Vec<(Bool, TraceExpr)>) -> Self {
        StringExpr { switches: sw }
    }
}

impl From<Vec<Conjunct>> for Bool {
    fn from(cs: Vec<Conjunct>) -> Self {
        Bool(cs)
    }
}

impl From<Conjunct> for Bool {
    fn from(conj: Conjunct) -> Self {
        Bool(vec![conj])
    }
}

impl From<Vec<Predicate>> for Conjunct {
    fn from(ps: Vec<Predicate>) -> Self {
        Self(ps)
    }
}

impl From<Predicate> for Conjunct {
    fn from(ps: Predicate) -> Self {
        Self(vec![ps])
    }
}

impl From<AtomicExpr> for TraceExpr {
    fn from(value: AtomicExpr) -> Self {
        Self(vec![value])
    }
}

impl From<Vec<AtomicExpr>> for TraceExpr {
    fn from(fs: Vec<AtomicExpr>) -> Self {
        TraceExpr(fs)
    }
}

impl From<Vec<Token>> for RegularExpr {
    fn from(ts: Vec<Token>) -> Self {
        Self(ts)
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
        write!(f, "Switch(")?;
        for (i, (b, e)) in self.switches.iter().enumerate() {
            write!(f, "({}, {})", b, e)?;
            if i != self.switches.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
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
        write!(f, "({}, {}, {})", self.v, self.r, self.k)
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
        if self.is_empty() {
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
            Token::ExcludeOneOrMore(tc) => write!(f, "!{}+", tc),
        }
    }
}

impl Display for Special {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Special::Is(c) => write!(f, "{c}"),
            Special::Not(c) => write!(f, "!{c}"),
            Special::Start => write!(f, "start"),
            Special::End => write!(f, "end"),
        }
    }
}

impl Display for TokenClass {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TokenClass::NumTok => write!(f, "NumTok"),
            TokenClass::AlphaTok => write!(f, "AlphaTok"),
            TokenClass::UpperTok => write!(f, "UpperTok"),
            TokenClass::LowerTok => write!(f, "LowerTok"),
            TokenClass::WhileSpaceTok => write!(f, "NonSpaceTok"),
        }
    }
}