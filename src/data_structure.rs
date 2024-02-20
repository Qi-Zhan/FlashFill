//! Implementation of data-structure that manipulating the expression
//!
//! Corresponds to the Figure 3, 4 of the paper

use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::language::*;

/// trait for data structure
pub trait IntersectSize {
    /// Option for empty set
    fn intersect(&self, other: &Self) -> Option<Self>
    where
        Self: std::marker::Sized;
    fn size(&self) -> usize;

    fn unify(&self, other: &Self) -> Option<Self>
    where
        Self: std::marker::Sized;

    type Instance;
    /// instantiation to get all programs
    fn all(&self) -> Vec<Self::Instance>;
}

/// A set of string programs
#[derive(Debug, Clone)]
pub struct ProgramSet(pub Vec<(Bool, Dag)>);

pub type Node = usize;
pub type Edge = (usize, usize);

/// A set of trace programs, tricky one
#[derive(Debug, Clone)]
pub struct Dag {
    pub nodes: HashSet<Node>,
    pub source: Node,
    pub target: Node,
    pub w: HashMap<(Node, Node), Vec<AtomicSet>>,
}

#[derive(Debug, Clone)]
pub enum AtomicSet {
    SubStr {
        v: FreeString,
        p1: Vec<PositionSet>,
        p2: Vec<PositionSet>,
    },
    ConstStr(String),
    Loop {
        e: Dag,
    },
}

#[derive(Debug, Clone)]
pub enum PositionSet {
    CPos(i32),
    Pos {
        r1: RegularSet,
        r2: RegularSet,
        c: HashSet<IntegerExpr>,
    },
}

#[derive(Debug, Clone)]
pub struct RegularSet(pub Vec<HashSet<Token>>);

impl IntersectSize for ProgramSet {
    /// no need to intersect string set?
    fn intersect(&self, _other: &Self) -> Option<Self> {
        unreachable!()
    }

    fn unify(&self, _other: &Self) -> Option<Self> {
        unreachable!()
    }

    fn size(&self) -> usize {
        self.0.iter().fold(1, |acc, x| acc * x.1.size())
    }

    type Instance = StringExpr;

    fn all(&self) -> Vec<Self::Instance> {
        let switches = &self.0;
        let bools = switches.iter().map(|(b, _)| b.clone());
        let es = switches.iter().map(|f| f.1.all());
        es.multi_cartesian_product()
            .map(|f| StringExpr(bools.clone().zip(f).collect_vec()))
            .collect_vec()
    }
}

/// util struct to map (usize, usize) to usize
struct MapUtil {
    w: usize,
}

impl MapUtil {
    fn new(h: usize) -> Self {
        Self { w: h }
    }

    fn get(&self, index0: usize, index1: usize) -> usize {
        index1 * self.w + index0
    }
}

impl IntersectSize for Dag {
    fn unify(&self, other: &Self) -> Option<Self> {
        let map = MapUtil::new(self.target - self.source + 1);
        let source = map.get(self.source, other.source);
        let target = map.get(self.target, other.target);
        let nodes: HashSet<Node> = HashSet::from_iter(source..=target);
        let mut w = HashMap::new();
        for (&(i1, i2), w1) in &self.w {
            for (&(j1, j2), w2) in &other.w {
                let k1 = map.get(i1, j1);
                let k2 = map.get(i2, j2);
                let f12 = w1
                    .iter()
                    .flat_map(|f1| w2.iter().flat_map(|f2| f1.unify(f2)).collect_vec())
                    .collect_vec();
                if !f12.is_empty() {
                    w.insert((k1, k2), f12);
                }
            }
        }
        let dag = Dag {
            nodes,
            source,
            target,
            w,
        };
        // We cannot tell the intersection is valid if there exist no valid path from src to target
        if !dag.exists_path() {
            return None;
        }
        Some(dag)
    }

    fn intersect(&self, other: &Self) -> Option<Self> {
        let map = MapUtil::new(self.target - self.source + 1);
        let source = map.get(self.source, other.source);
        let target = map.get(self.target, other.target);
        let nodes: HashSet<Node> = HashSet::from_iter(source..=target);
        let mut w = HashMap::new();
        for (&(i1, i2), w1) in &self.w {
            for (&(j1, j2), w2) in &other.w {
                let k1 = map.get(i1, j1);
                let k2 = map.get(i2, j2);
                let f12 = w1
                    .iter()
                    .flat_map(|f1| w2.iter().flat_map(|f2| f1.intersect(f2)).collect_vec())
                    .collect_vec();
                if !f12.is_empty() {
                    w.insert((k1, k2), f12);
                }
            }
        }
        let dag = Dag {
            nodes,
            source,
            target,
            w,
        };
        // We cannot tell the intersection is valid if there exist no valid path from src to target
        if !dag.exists_path() {
            return None;
        }
        Some(dag)
    }

    fn size(&self) -> usize {
        self.node_size(self.target)
    }

    type Instance = TraceExpr;

    fn all(&self) -> Vec<Self::Instance> {
        self.node_all(self.target)
            .expect("valid program not exists")
            .into_iter()
            .map(TraceExpr)
            .collect_vec()
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
                    let v = *v1;
                    let p1: Vec<_> = p11
                        .iter()
                        .cartesian_product(p21.iter())
                        .flat_map(|(a, b)| a.intersect(b))
                        .collect();
                    if p1.is_empty() {
                        return None;
                    }
                    let p2: Vec<_> = p12
                        .iter()
                        .cartesian_product(p22.iter())
                        .flat_map(|(a, b)| a.intersect(b))
                        .collect();
                    if p2.is_empty() {
                        return None;
                    }
                    Some(AtomicSet::SubStr { v, p1, p2 })
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

    fn unify(&self, other: &Self) -> Option<Self> {
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
                    let v = *v1;
                    let p1: Vec<_> = p11
                        .iter()
                        .cartesian_product(p21.iter())
                        .flat_map(|(a, b)| a.unify(b))
                        .collect();
                    if p1.is_empty() {
                        return None;
                    }
                    let p2: Vec<_> = p12
                        .iter()
                        .cartesian_product(p22.iter())
                        .flat_map(|(a, b)| a.unify(b))
                        .collect();
                    if p2.is_empty() {
                        return None;
                    }
                    Some(AtomicSet::SubStr { v, p1, p2 })
                }
            }
            _ => None,
        }
    }

    fn size(&self) -> usize {
        match self {
            AtomicSet::SubStr { v: _, p1, p2 } => {
                p1.iter().map(|f| f.size()).sum::<usize>()
                    * p2.iter().map(|f| f.size()).sum::<usize>()
            }
            AtomicSet::ConstStr(_) => 1,
            AtomicSet::Loop { e } => e.size(),
        }
    }

    type Instance = AtomicExpr;

    fn all(&self) -> Vec<Self::Instance> {
        match self {
            AtomicSet::SubStr { v, p1, p2 } => {
                let p1 = p1.iter().flat_map(|p| p.all());
                let p2 = p2.iter().flat_map(|p| p.all());
                p1.cartesian_product(p2)
                    .map(|(p1, p2)| AtomicExpr::SubStr { v: *v, p1, p2 })
                    .collect_vec()
            }
            AtomicSet::Loop { e } => e
                .all()
                .into_iter()
                .map(|e| AtomicExpr::Loop { e })
                .collect_vec(),
            AtomicSet::ConstStr(s) => vec![AtomicExpr::ConstStr(s.to_string())],
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
                PositionSet::Pos { r1, r2, c: c1 },
                PositionSet::Pos {
                    r1: r3,
                    r2: r4,
                    c: c2,
                },
            ) => Some(PositionSet::Pos {
                r1: r1.intersect(r3)?,
                r2: r2.intersect(r4)?,
                c: c1.intersect(c2)?,
            }),
            _ => None,
        }
    }

    fn unify(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (
                PositionSet::Pos { r1, r2, c: _ },
                PositionSet::Pos {
                    r1: r3,
                    r2: r4,
                    c: _,
                },
            ) => {
                let r1 = r1.intersect(r3)?;
                let r2 = r2.intersect(r4)?;
                Some(PositionSet::Pos {
                    r1,
                    r2,
                    c: HashSet::from([IntegerExpr::Bound]),
                })
            }
            _ => None,
        }
    }

    fn size(&self) -> usize {
        match self {
            PositionSet::CPos(_) => 1,
            PositionSet::Pos { r1, r2, c } => r1.size() * r2.size() * c.len(),
        }
    }

    type Instance = Position;

    fn all(&self) -> Vec<Self::Instance> {
        match self {
            &PositionSet::CPos(k) => vec![Position::CPos(k)],
            PositionSet::Pos { r1, r2, c } => {
                let r1 = r1.all();
                let r2 = r2.all();
                r1.into_iter()
                    .cartesian_product(r2)
                    .cartesian_product(c.iter())
                    .map(|((r1, r2), c)| Position::Pos { r1, r2, c: *c })
                    .collect()
            }
        }
    }
}

impl IntersectSize for RegularSet {
    fn intersect(&self, other: &Self) -> Option<Self> {
        if self.0.len() != other.0.len() {
            None
        } else {
            let tokens = self
                .0
                .iter()
                .zip(other.0.iter())
                .map(|(t1, t2)| t1.intersect(t2))
                .collect_vec();
            // may exist more elegant way to return when None result exists
            let mut new_tokens = vec![];
            for token in tokens {
                match token {
                    Some(token) => new_tokens.push(token),
                    None => return None,
                }
            }
            Some(Self(new_tokens))
        }
    }

    fn size(&self) -> usize {
        self.0.iter().map(|f| f.len()).product::<usize>()
    }

    type Instance = RegularExpr;

    fn all(&self) -> Vec<Self::Instance> {
        let tokens = &self.0;
        // special for empty regex
        if tokens.len() == 1 && tokens[0].is_empty() {
            return vec![RegularExpr::empty()];
        }
        tokens
            .iter()
            .multi_cartesian_product()
            .map(|f| RegularExpr(f.into_iter().copied().collect_vec()))
            .collect_vec()
    }

    fn unify(&self, other: &Self) -> Option<Self> {
        self.intersect(other)
    }
}

impl<T: Copy + Eq + std::hash::Hash> IntersectSize for HashSet<T> {
    fn intersect(&self, other: &Self) -> Option<Self> {
        // empty set is NOT EQUAL to None
        if self.is_empty() && other.is_empty() {
            return Some(self.clone());
        }
        let result: HashSet<_> = self.intersection(other).copied().collect();
        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }

    fn size(&self) -> usize {
        self.len()
    }

    type Instance = T;

    fn all(&self) -> Vec<Self::Instance> {
        self.iter().copied().collect_vec()
    }

    fn unify(&self, other: &Self) -> Option<Self> {
        self.intersect(other)
    }
}

impl Dag {
    fn node_size(&self, node: Node) -> usize {
        if node == 0 {
            1
        } else {
            self.w
                .iter()
                .map(|(&(src, tgt), fs)| -> usize {
                    if tgt == node {
                        self.node_size(src) * fs.iter().map(|f| f.size()).sum::<usize>()
                    } else {
                        0
                    }
                })
                .sum()
        }
    }

    /// interesting one
    ///
    /// Return all patch from source to node
    fn node_all(&self, node: Node) -> Option<Vec<Vec<AtomicExpr>>> {
        if node == self.source {
            return Some(vec![]);
        }
        let paths = self
            .w
            .iter()
            .filter_map(|((src, tgt), fs)| {
                if *tgt == node {
                    self.node_all(*src).map(|paths| {
                        let f_all = fs.iter().flat_map(|f| f.all());
                        // Some(vec![])
                        if paths.is_empty() {
                            f_all.map(|f| vec![f]).collect_vec()
                        } else {
                            paths
                                .into_iter()
                                .cartesian_product(f_all)
                                .map(|(path, f)| {
                                    let mut path = path.clone();
                                    path.push(f);
                                    path
                                })
                                .collect_vec()
                        }
                    })
                } else {
                    None
                }
            })
            .collect_vec();
        if paths.is_empty() {
            None
        } else {
            Some(paths.concat())
        }
    }

    fn exists_path(&self) -> bool {
        let edges = self.w.keys().collect_vec();
        let mut visited = HashSet::new();
        let mut queue = vec![self.target];
        while let Some(item) = queue.pop() {
            if item == 0 {
                return true;
            }
            if visited.insert(item) {
                for (i, j) in &edges {
                    if *j == item {
                        queue.push(*i);
                    }
                }
            }
        }
        false
    }

    /// Definition 3: compatible
    pub(crate) fn comp(&self, other: &Self) -> bool {
        self.intersect(other).is_some()
    }

    /// Definition 4: compatible score
    pub(crate) fn cs<T>(index: (usize, usize), t: &[(T, Self)]) -> (usize, f32) {
        let (i1, i2) = (index.0, index.1);
        let (e1, e2) = (&t[i1].1, &t[i2].1);
        (Self::cs1(t, i1, i2), e1.cs2(e2))
    }

    /// Definition 4: compatible score1
    fn cs1<T>(t: &[(T, Self)], i1: usize, i2: usize) -> usize {
        t.iter()
            .enumerate()
            .map(|(i, _)| {
                if i == i1 || i == i2 {
                    0
                } else {
                    t[i1].1.z(&t[i2].1, &t[i].1)
                }
            })
            .sum()
    }

    /// Definition 4: compatible score2
    fn cs2(&self, other: &Self) -> f32 {
        self.intersect(other).unwrap().size() as f32 / self.size().max(other.size()) as f32
    }

    /// Definition 4: z
    fn z(&self, other: &Self, k: &Self) -> usize {
        if self.comp(k) == other.comp(k) && self.comp(k) == k.comp(&self.intersect(other).unwrap())
        {
            1
        } else {
            0
        }
    }
}
