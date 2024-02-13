//! Implementation of data-structure that manipulating the expression
//!
//! Corresponds to the Figure 3, 4 of the paper

use std::collections::HashSet;

use crate::language::{Bool, FreeString, IntegerExpr};

/// trait for data structure
pub trait IntersectSize {
    /// Option for empty set
    fn intersect(&self, other: &Self) -> Option<Self>
    where
        Self: std::marker::Sized;
    fn size(&self) -> usize;
}

/// A set of string programs
pub struct StringSet(Vec<(Bool, Dag)>);

/// A set of trace programs, tricky one
pub struct Dag {}

pub enum AtomicSet {
    SubStr {
        v: FreeString,
        p1: PositionSet,
        p2: PositionSet,
    },
    ConstStr(String),
    Loop {
        e: Dag,
    },
}

pub enum PositionSet {
    CPos(i32),
    Pos {
        r1: RegularSet,
        r2: RegularSet,
        c: HashSet<IntegerExpr>,
    },
}

pub struct RegularSet {}

impl IntersectSize for StringSet {
    /// no need to intersect string set?
    fn intersect(&self, _other: &Self) -> Option<Self> {
        todo!()
    }

    fn size(&self) -> usize {
        self.0.iter().fold(1, |acc, x| acc * x.1.size())
    }
}

impl IntersectSize for Dag {
    fn intersect(&self, other: &Self) -> Option<Self> {
        todo!()
    }

    fn size(&self) -> usize {
        todo!()
    }
}

impl IntersectSize for AtomicSet {
    fn intersect(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (
                AtomicSet::SubStr {
                    v: v1,
                    p1: p11,
                    p2: p12,
                },
                AtomicSet::SubStr {
                    v: v2,
                    p1: p21,
                    p2: p22,
                },
            ) => {
                if v1 != v2 {
                    None
                } else {
                    todo!()
                }
            }
            (AtomicSet::ConstStr(s1), AtomicSet::ConstStr(s2)) => {
                if s1 == s2 {
                    Some(AtomicSet::ConstStr(s1.clone()))
                } else {
                    None
                }
            }
            (AtomicSet::Loop { e: e1 }, AtomicSet::Loop { e: e2 }) => Some(AtomicSet::Loop {
                e: e1.intersect(e2)?,
            }),
            _ => None,
        }
    }

    fn size(&self) -> usize {
        match self {
            AtomicSet::SubStr { v: _, p1, p2 } => todo!(),
            AtomicSet::ConstStr(_) => 1,
            AtomicSet::Loop { e } => e.size(),
        }
    }
}

impl IntersectSize for PositionSet {
    fn intersect(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (PositionSet::CPos(k1), PositionSet::CPos(k2)) => {
                if k1 == k2 {
                    Some(PositionSet::CPos(*k1))
                } else {
                    None
                }
            }
            (
                PositionSet::Pos { r1, r2, c },
                PositionSet::Pos {
                    r1: r3,
                    r2: r4,
                    c: c_,
                },
            ) => todo!(),
            _ => None,
        }
    }

    fn size(&self) -> usize {
        match self {
            PositionSet::CPos(_) => 1,
            PositionSet::Pos { r1, r2, c } => r1.size() * r2.size() * c.len(),
        }
    }
}

impl IntersectSize for RegularSet {
    fn intersect(&self, other: &Self) -> Option<Self> {
        todo!()
    }

    fn size(&self) -> usize {
        todo!()
    }
}
