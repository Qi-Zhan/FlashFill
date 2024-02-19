//! Synthesis algorithm, corresponds Fig.7 in the paper

use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::data_structure::*;
use crate::eval::{Eval, EvalBool};
use crate::language::*;
use crate::util::InputState;

pub fn generate_string_program(pairs: &[(InputState, String)]) -> StringExpr {
    // Step1: Generate TraceExpr for every input-output example
    let trace = pairs
        .iter()
        .map(|(sigma, s)| (sigma.clone(), generate_str(sigma, s, true)))
        .collect_vec();
    // Step2
    let trace = generate_partitions(trace);
    let sigmas: HashSet<_> = trace.iter().map(|f| &f.0).collect();
    // Step3: Geneate Bool for every partition
    let mut switches: Vec<_> = trace
        .clone()
        .into_iter()
        .map(|(sigma, e)| {
            let mut sigma2 = sigmas.clone();
            sigma2.remove(&sigma);
            let b = generate_bool_classifier(&sigma, &sigma2);
            (b, e)
        })
        .collect();
    switches.sort_by_key(|dag| dag.1.size());
    let program_set = ProgramSet(switches);
    program_set
        .all()
        .into_iter()
        .next()
        .expect("[ERROR] valid program not !")
}

fn combination2(len: usize) -> impl Iterator<Item = (usize, usize)> {
    let mut v = vec![];
    for i in 0..len {
        for j in i + 1..len {
            v.push((i, j))
        }
    }
    v.into_iter()
}

fn exist_comp<T>(t: &[(T, Dag)]) -> bool {
    for (i, j) in combination2(t.len()) {
        if t[i].1.comp(&t[j].1) {
            return true;
        }
    }
    false
}

fn generate_partitions(t: Vec<(InputState, Dag)>) -> Vec<(Vec<InputState>, Dag)> {
    let mut t = t
        .into_iter()
        .map(|(input, dag)| (vec![input], dag))
        .collect_vec();
    while exist_comp(&t) {
        let (i1, i2) = combination2(t.len())
            .map(|index| (index, Dag::cs(index, &t)))
            .max_by(|(_, a), (_, b)| {
                if a.0 == b.0 {
                    a.1.partial_cmp(&b.1).unwrap()
                } else {
                    a.0.cmp(&b.0)
                }
            })
            .map(|(i, _)| i)
            .unwrap();
        // i2 > i1 so there is no indices problem
        let (s2, e2) = t.remove(i2);
        let (s1, e1) = t.remove(i1);
        let mut s = s1;
        s.extend(s2);
        let e = e1.intersect(&e2).unwrap();
        t.push((s, e))
    }
    t
}

fn all_predicates(len: usize) -> Vec<Predicate> {
    let times = 1..=3;
    let range = 0..len;
    ALLTOKENSEQS
        .iter()
        .cartesian_product(range)
        .cartesian_product(times)
        .flat_map(|((r, v), k)| {
            if r.0.is_empty() {
                vec![]
            } else {
                vec![
                    Predicate::Is(Match::new(v, r.clone(), k)),
                    Predicate::Not(Match::new(v, r.clone(), k)),
                ]
            }
        })
        .collect_vec()
}

fn choose_predicate(
    all_predicates: &[Predicate],
    sigma1: &HashSet<&InputState>,
    sigma2: &HashSet<&InputState>,
) -> Predicate {
    all_predicates
        .iter()
        .max_by_key(|p| csp(p, sigma1, sigma2))
        .unwrap()
        .clone()
}

/// Definition 5: Classfication Score of a Predicate
fn csp(p: &Predicate, sigma1: &HashSet<&InputState>, sigma2: &HashSet<&InputState>) -> usize {
    sigma1.iter().filter(|s| p.eval(s)).count() * sigma2.iter().filter(|s| !p.eval(s)).count()
}

fn generate_bool_classifier(sigma1: &[Vec<String>], sigma2: &HashSet<&Vec<InputState>>) -> Bool {
    let all_matches = all_predicates(sigma1[0].len());
    let mut s1_prime: HashSet<_> = sigma1.iter().collect();
    let mut b = vec![];
    while !s1_prime.is_empty() {
        let old1 = s1_prime.clone();
        let mut s2_prime: HashSet<_> = sigma2.iter().copied().flatten().collect();
        let mut s1_prime_prime = s1_prime.clone();
        let mut d = vec![];
        while !s2_prime.is_empty() {
            let old2 = s2_prime.clone();
            let p = choose_predicate(&all_matches, &s1_prime_prime, &s2_prime);
            // only keep sat
            s1_prime_prime.retain(|input| p.eval(input));
            s2_prime.retain(|input| p.eval(input));
            if old2 == s2_prime {
                panic!("generate bool classsifier fail")
            }
            d.push(p);
        }
        s1_prime = s1_prime.difference(&s1_prime_prime).copied().collect();
        b.push(Conjunct(d));
        if old1 == s1_prime {
            panic!("generate bool classsifier fail")
        }
    }
    Bool(b)
}

fn generate_str(sigma: &InputState, s: &str, loop_: bool) -> Dag {
    let source = 0;
    let target = s.len();
    let nodes: HashSet<Node> = HashSet::from_iter(source..=target);
    let mut w = HashMap::new();

    for i in 0..s.len() {
        for j in (i + 1)..=s.len() {
            // ConstStr[i..j]
            let con_str = AtomicSet::ConstStr(s[i..j].to_string());
            // GenerateSubString\[i..j\]
            let mut atmoics = generate_substring(sigma, &s[i..j]);
            atmoics.push(con_str);
            w.insert((i, j), atmoics);
        }
    }
    if loop_ {
        w = generate_loop(sigma, s, w);
    }
    Dag {
        nodes,
        source,
        target,
        w,
    }
}

fn generate_loop(
    sigma: &InputState,
    s: &str,
    mut w: HashMap<Edge, Vec<AtomicSet>>,
) -> HashMap<Edge, Vec<AtomicSet>> {
    for k1 in 0..s.len() {
        for k2 in k1..s.len() {
            'outer: for k3 in k2..s.len() {
                let e1 = generate_str(sigma, &s[k1..=k2], false); // only recursive once
                let e2 = generate_str(sigma, &s[k2..=k3], false); // only recursive once
                let e = e1.unify(&e2);
                if let Some(e) = e {
                    for trace in e.all() {
                        let eval = AtomicExpr::Loop { e: trace }.eval(sigma).unwrap();
                        if s[k1..].starts_with(&eval) {
                            let k4 = k1 + eval.len();
                            w.entry((k1, k4))
                                .and_modify(|f| f.push(AtomicSet::Loop { e: e.clone() }));
                            continue 'outer;
                        }
                    }
                }
            }
        }
    }
    w
}

fn generate_substring(sigma: &InputState, s: &str) -> Vec<AtomicSet> {
    let mut results = vec![];
    for (i, input) in sigma.iter().enumerate() {
        let matches = input.match_indices(s);
        for (k, _) in matches {
            let y1 = generate_position(input, k as i32);
            let y2 = generate_position(input, (k + s.len()) as i32);
            results.push(AtomicSet::SubStr {
                v: i,
                p1: y1,
                p2: y2,
            });
        }
    }
    results
}

fn generate_position(s: &str, k: i32) -> Vec<PositionSet> {
    let mut results = vec![
        PositionSet::CPos(k),
        // PAPER WRONG HERE
        PositionSet::CPos(-(s.len() as i32 - k) - 1),
    ];
    for r1 in ALLTOKENSEQS.iter() {
        for r2 in ALLTOKENSEQS.iter() {
            if r1.0.is_empty() && r2.0.is_empty() {
                continue;
            }
            for (k1, _) in r1.find_all_matches(s).filter(|(_, b)| *b == k - 1) {
                for (_, k2) in r2.find_all_matches(s).filter(|(a, _)| *a == k) {
                    let r12 = r1.concat(r2);
                    // even r1 match, r2 match, r12 is not always match at [k1, k2]
                    if let Some(c) = r12
                        .find_all_matches(s)
                        .find_position(|(a, b)| *a == k1 && *b == k2)
                        .map(|(index, _)| index + 1)
                    {
                        let c_all = r12.find_all_matches(s).count();
                        let r1 = generate_regex(r1, s);
                        let r2 = generate_regex(r2, s);
                        results.push(PositionSet::Pos {
                            r1,
                            r2,
                            c: HashSet::from([
                                IntegerExpr::Constant(c as i32),
                                IntegerExpr::Constant(-((c_all + 1 - c) as i32)),
                            ]),
                        })
                    }
                }
            }
        }
    }
    results
}

fn generate_regex(r: &RegularExpr, _s: &str) -> RegularSet {
    if r.0.is_empty() {
        return RegularSet(vec![HashSet::new()]);
    }
    let tokenseqs =
        r.0.iter()
            .map(|token| HashSet::from([*token]))
            .collect_vec();
    RegularSet(tokenseqs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{examples::EXAMPLE2, splits};

    #[test]
    fn test_gen_position() {
        let s = "BTR KRNL WK CORN 15Z";
        let position_sets1 = generate_position(s, 17);
        let s = "CAMPDRY DBL NDL3.6 OZ";
        let position_sets2 = generate_position(s, 15);
        let intersection = position_sets1
            .iter()
            .cartesian_product(position_sets2.iter())
            .flat_map(|(a, b)| a.intersect(b))
            .collect_vec();
        // Pos(empty, Num, 1)
        assert_eq!(intersection.len(), 1);
    }

    #[test]
    fn test_generate_str() {
        let test_data = splits(&EXAMPLE2);
        let trace = test_data
            .iter()
            .map(|(sigma, s)| (generate_str(sigma, s, false)))
            .collect_vec();
        let dag1 = &trace[0];
        let dag2 = &trace[1];
        let a1 = &dag1.w[&(dag1.source, dag1.target)];
        let a2 = &dag2.w[&(dag2.source, dag2.target)];
        let a = &a1[0];
        let b = &a2[0];
        assert!(a.intersect(b).is_some());
    }
}
