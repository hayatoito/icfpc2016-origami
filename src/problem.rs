use super::prelude::*;

use super::contest;
use super::fileio;

use std;
use std::fs;
use std::str::FromStr;

use regex::Regex;

use num::bigint::BigInt;
use num::ToPrimitive;
use num::rational::BigRational;

use serde_json;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Silhouette {
    pub polygons: Vec<Polygon>,
    pub skeltons: Vec<Edge>,
}

impl Silhouette {

    pub fn is_simple(&self) -> bool {
        self.polygons.len() == 1 && self.polygons[0].len() == 4
    }
}

#[test]
fn bigint_test() {
    // 4328029871649615121465353437184/8656059743299229793415925725865
    let a = BigInt::parse_bytes(b"4328029871649615121465353437184", 10).unwrap();
    // assert_eq!(&format!("a: {}", a), "");
    let b = BigInt::parse_bytes(b"8656059743299229793415925725865", 10).unwrap();
    // println!("b: {}", b);

    let c = BigRational::new(a, b);
    let f = bigrational_to_f64(c);
    assert_eq!(f, 0.5);
}

impl FromStr for Silhouette {

    type Err = ();

    fn from_str(s: &str) -> Result<Silhouette, Self::Err> {
        let f = std::io::BufReader::new(s.as_bytes());
        let mut s = fileio::Scanner::new(f);

        let num_polygons: usize = s.next();
        debug!("num_polygons: {}", num_polygons);
        let mut polygons: Vec<Polygon> = Vec::with_capacity(num_polygons);
        for _ in 0..num_polygons {
            let num_verticles: usize = s.next();
            debug!("num_verticles: {}", num_verticles);
            let ps = (0..num_verticles).map(|_| {
                let cood: Point = s.next();
                cood
            }).collect::<Vec<Point>>();
            polygons.push(Polygon::new(ps));
        }

        let num_skelton_edges: usize = s.next();
        debug!("num_skilton_edges: {}", num_skelton_edges);
        let edges = (0..num_skelton_edges).map(|_| {
            Edge { a: s.next(), b: s.next() }
        }).collect::<Vec<Edge>>();

        Ok(Silhouette {
            polygons: polygons,
            skeltons: edges
        })
    }
}

pub fn all_problems() -> Vec<(usize, Silhouette)> {
    let re = Regex::new(r"^.*/problem-(\d{4})\.txt$").unwrap();

    let dir = format!("{}/problems/", env!("CARGO_MANIFEST_DIR"));
    let mut problems = vec![];
    for entry in fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        let s = path.to_str().unwrap();
        println!("processing: {:?}", s);
        if let Some(caps) = re.captures(s) {
            let id: usize = caps.at(1).unwrap().parse().unwrap();
            // silhouette: Silhouette = fileio::read(s).parse().unwrap();
            let silhouette: Result<Silhouette, ()> = fileio::read(s).parse();
            match silhouette {
                Ok(silhouette) => {
                    debug!("silhouette: {:?}", silhouette);
                    problems.push((id, silhouette));
                },
                _ => {
                    warn!("Can not parse: {:?}", silhouette);
                }
            }
        }
    }
    problems.sort_by_key(|&(id, _)| id);
    problems
}

pub fn update_problems_json() {
    for (id, silhouette) in all_problems() {
        debug!("silhouette: {:?}", silhouette);
        // silhouette.debug_info();
        // let j: SilhouetteJ = silhouette.into();
        let json = serde_json::to_string_pretty(&silhouette).unwrap();
        debug!("json: {}", serde_json::to_string_pretty(&silhouette).unwrap());
        fileio::write_json(&json, &format!("problem-{:04}.json", id));
    }
}

pub fn show_simple_problems() {
    for (id, silhouette) in all_problems() {
        if silhouette.is_simple() {
            println!("simple problem: {}", id)
        }
    }
}
