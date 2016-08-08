use super::prelude::*;

use super::fileio;
use super::solution;
use super::problem;

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::btree_map::Entry::*;

use std;
use std::ops::*;

use serde_json;

struct Solver {
    origami: Origami,
    silhouette: problem::Silhouette,
}

impl Solver {
    fn score(&self) -> f64 {
        let (facet, sources) = self.origami.union();
        let sil = Facet::from_polygon(&self.silhouette.polygons[0], sources);
        let sources = sources + sil.len();

        let (intersection, _) = facet.intersection(&sil, sources);
        match facet.union(&sil, sources) {
            Some((union, _)) => intersection.area() / union.area(),
            None => intersection.area() / (facet.area() + sil.area())
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Origami {
    facets: Vec<FoldedFacet>,
    source_size: usize,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FoldedFacet {
    facet: Facet,
    fold_history: Vec<Line>,
}

impl Deref for FoldedFacet {
    type Target = [SourcePoint];
    fn deref(&self) -> &Self::Target {
        self.facet.deref()
    }
}

impl FoldedFacet {

    fn new(facet: Facet, fold_history: Vec<Line>) -> FoldedFacet {
        FoldedFacet { facet: facet, fold_history: fold_history }
    }

    fn fold(&self, m: Line, sources: usize) -> (Vec<FoldedFacet>, usize) {
        let mut v = vec![];
        let CutResult{ left, right, sources } = self.facet.cut(m, sources);
        if !left.is_empty() {
            v.push(FoldedFacet::new(left, self.fold_history.clone()));
        }
        if !right.is_empty() {
            let mut nv = self.fold_history.clone();
            nv.push(m);
            v.push(FoldedFacet::new(right.reflect_on(m), nv));
        }
        (v, sources)
    }

    fn unfold(&self) -> Facet {
        self.fold_history.iter().rev().fold(self.facet.clone(), |facet, line| facet.reflect_on(*line))
    }

}

impl Origami {

    fn new(facets: Vec<FoldedFacet>, source_size: usize) -> Origami {
        Origami { facets: facets, source_size: source_size }
    }

    fn fold(&self, m: Line) -> Origami {
        let (facets, sources) = self.facets.iter().fold((vec![], self.source_size), |(facets, source_size), f| {
            let (mut fs, sources) = f.fold(m, source_size);
            let mut facets = facets;
            facets.append(&mut fs);
            (facets, sources)
        });
        Origami::new(facets, sources)
    }

    fn area(&self) -> f64 {
        self.union().0.area()
    }

    fn collect_sorted_destination_points(&self) -> Vec<SourcePoint> {
        let mut source_map: BTreeMap<usize, SourcePoint> = BTreeMap::new();
        for f in &self.facets {
            for sp in f.iter() {
                let i = sp.index;
                match source_map.entry(i) {
                    Occupied(e) => debug_assert!(sp.p.approx_eq(&e.get().p)),
                    Vacant(e) => { e.insert(*sp); }
                }
            }
        }
        assert_eq!(source_map.len(), self.source_size);
        source_map.values().cloned().collect()
    }

    fn union(&self) -> (Facet, usize) {
        let mut facets = self.facets.iter().map(|f| f.facet.clone()).collect::<Vec<_>>();
        let mut sources = self.source_size;

        while facets.len() > 1 {
            let pre = facets.len();
            let f0 = facets.swap_remove(pre - 1);
            let (i, r) = facets.iter().enumerate().map(|(i, f)| (i, f0.union(f, sources))).find(|&(_, ref r)| r.is_some()).unwrap();
            let (f, s) = r.unwrap();
            sources = s;
            facets.swap_remove(i);
            facets.push(f);
        }
        (facets[0].clone(), sources)
    }

    fn unfold(&self) -> OrigamiUnfolded {
        let dest = self.collect_sorted_destination_points();
        OrigamiUnfolded {
            source_facets: self.facets.iter().map(|f| f.unfold()).collect(),
            source_size: self.source_size,
            destination_points: dest,
        }
    }

    // fn unison(&self) -> Facet {
    //     self.facets.fold(vec![], |(acc, f)|

    // }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct OrigamiUnfolded {
    source_facets: Vec<Facet>,
    source_size: usize,
    destination_points: Vec<SourcePoint>,  // sorted by sourceindex
}

pub fn run_origami() -> (Origami, OrigamiUnfolded) {
    let ps = vec![(0.0, 0.0),
                  (1.0, 0.0),
                  (1.0, 1.0),
                  (0.0, 1.0)];
    let facet = Facet::from_points(ps, 0);
    let origami = Origami::new(vec![FoldedFacet::new(facet, vec![])], 4);

    let m = Line::new(Point::new(0.5, 0.0), Point::new(1.0, 0.5));
    let origami = origami.fold(m);

    let m = Line::new(Point::new(1.5, 0.0), Point::new(1.0, -0.5));
    let origami = origami.fold(m);

    let m = Line::new(Point::new(1.8, -2.0), Point::new(0.0, 1.8));
    let origami = origami.fold(m);

    let unfold = origami.unfold();
    (origami, unfold)
}

pub fn run_simple_solver() {
    for (id, sil) in problem::all_problems() {
        if 101 < id && sil.is_simple() {
            if let Some(response) = fileio::read_solution_response(id) {
                if let Ok(r) = serde_json::from_str::<solution::SolutionResponse>(&response) {
                    println!("valid response already exists for {}. score: {}",
                             id, r.resemblance);
                    continue;
                }
            }

            // let sps = sil.polygons[0].0.iter().enumerate()
            //     .map(|(i, p)| SourcePoint::new(*p, i)).collect::<Facet>();

            // let facet = Facet::new(sps, Vec::new());
            // let origami = Origami::new(vec![facet], 4);

            // let convex = origami.convex_hull();
            // let area = area_of_convex(&convex);

            // println!("id: {}, area: {}", id, area);

            // println!("Making solution for: {}", id);

            // let problem_spec_text = fileio::read_problem_spec(id);;
            // let problem_lines = problem_spec_text.lines().collect::<Vec<_>>();

            // let mut solution_spec = String::new();
            // solution_spec.push_str("4\n");
            // solution_spec.push_str("0,0\n");
            // solution_spec.push_str("1,0\n");
            // solution_spec.push_str("1,1\n");
            // solution_spec.push_str("0,1\n");

            // solution_spec.push_str("1\n");

            // solution_spec.push_str("4 0 1 2 3\n");

            // solution_spec.push_str(problem_lines[2]);
            // solution_spec.push('\n');
            // solution_spec.push_str(problem_lines[3]);
            // solution_spec.push('\n');
            // solution_spec.push_str(problem_lines[4]);
            // solution_spec.push('\n');
            // solution_spec.push_str(problem_lines[5]);
            // solution_spec.push('\n');

            // fileio::write_solution_spec_generated(&solution_spec, id);
            // let path = fileio::path_solution_spec_generated(id);
            // solution::submit(id, &path);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use prelude::*;

    #[test]
    fn folded_facet_test() {
        let f = FoldedFacet::new(Facet::new_square(), vec![]);
        assert_eq!(f.facet.area(), 1.0);
        let sources = 4;
        let (v0, sources) = f.fold(Line::new(Point::new(0.5, -10.0), Point::new(0.5, 10.0)), sources);
        let f0 = &v0[0];
        let f1 = &v0[1];
        assert_eq!(sources, 6);
        assert_eq!(f0.len(), 4);
        assert_eq!(f1.len(), 4);
        assert_eq!(f0.facet.area(), 0.5);
        assert_eq!(f1.facet.area(), 0.5);

        let (v1, sources) = f0.fold(Line::new(Point::new(10.0, 0.5), Point::new(-10.0, 0.5)), sources);
        assert_eq!(sources, 8);
        let (v2, sources) = f1.fold(Line::new(Point::new(10.0, 0.5), Point::new(-10.0, 0.5)), sources);
        assert_eq!(sources, 10);
        let f2 = &v1[0];
        let f3 = &v1[1];
        let f4 = &v2[0];
        let f5 = &v2[1];

        assert_eq!(f2.facet.area(), 0.25);
        assert_eq!(f3.facet.area(), 0.25);
        assert_eq!(f4.facet.area(), 0.25);
        assert_eq!(f5.facet.area(), 0.25);

        assert_eq!(f2.unfold().area(), 0.25);
        assert_eq!(f3.unfold().area(), 0.25);
        assert_eq!(f4.unfold().area(), 0.25);
        assert_eq!(f5.unfold().area(), 0.25);
    }

    #[test]
    fn origami_fold_test() {
        let origami = Origami::new(vec![FoldedFacet::new(Facet::new_square(), vec![])], 4);
        assert_eq!(origami.area(), 1.0);
        let origami = origami.fold(Line::new(Point::new(0.5, -10.0), Point::new(0.5, 10.0)));
        assert_eq!(origami.area(), 0.5);
        let origami = origami.fold(Line::new(Point::new(10.0, 0.5), Point::new(-10.0, 0.5)));
        assert_eq!(origami.area(), 0.25);
    }

}
