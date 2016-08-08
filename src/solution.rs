use std;

use super::prelude::*;
use super::fileio;
use super::netio;

use num::ToPrimitive;
use num::rational::BigRational;

#[derive(Debug, PartialEq)]
pub struct Solution {
    pub sources: Vec<Point>,
    pub facet: Vec<SolutionFacet>,
    pub destinations: Vec<Point>,
}

#[derive(Debug, PartialEq)]
pub struct SolutionFacet {
    pub ps: Vec<usize>,
}

impl std::fmt::Display for SolutionFacet {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = self.ps.iter().map(|i| i.to_string()).collect::<Vec<String>>();
        write!(f, "{} {}", self.ps.len(), s.join(" "))
    }
}

fn point_to_rational_str(p: &Point) -> String {
    format!("{},{}",
            BigRational::from_float(p.x).unwrap(),
            BigRational::from_float(p.y).unwrap())
}

impl Solution {

    pub fn to_spec_text(&self) -> String {
        let mut out = String::new();

        out.push_str(&self.sources.len().to_string());
        out.push('\n');
        for p in &self.sources {
            out.push_str(&point_to_rational_str(p));
            out.push('\n');
        }

        out.push_str(&self.facet.len().to_string());
        out.push('\n');
        for f in &self.facet {
            out.push_str(&f.to_string());
            out.push('\n');
        }
        for p in &self.destinations {
            out.push_str(&point_to_rational_str(p));
            out.push('\n');
        }
        out
    }
}

pub fn submit(id: usize, file: &str) {
    let spec = fileio::read(file);
    info!("file: {}", file);
    // info!("spec: {}", spec);
    let response = netio::submit_solution(id, file);
    println!("response: {}", response);
    fileio::write_solution_spec(&spec, id);
    fileio::write_solution_response(&response, id);
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct SolutionResponse {
    solution_size: usize,
    ok: bool,
    pub resemblance: f64,
    problem_id: usize,
    solution_spec_hash: String,
}
