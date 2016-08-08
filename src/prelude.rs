use euclid::approxeq::ApproxEq;
use num::FromPrimitive;
use num::ToPrimitive;
use num::bigint::BigInt;
use num::complex::Complex;
use num::rational::BigRational;
use ordered_float::OrderedFloat;
use rand;
use serde_json;
use std::cmp::Ord;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::btree_map::Entry::*;
use std::ops::*;
use std::str::FromStr;
use std;

#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
pub struct Point {
    pub x: f64,
    pub y: f64,
}

impl Point {
    pub fn new(x: f64, y: f64) -> Point {
        Point { x: x, y: y }
    }

    pub fn dot(self, other: Point) -> f64 {
        self.x * other.x + self.y * other.y
    }

    pub fn cross(self, other: Point) -> f64 {
        self.x * other.y - self.y * other.x
    }

    pub fn conjugate(self) -> Point {
        Point::new(self.x, -self.y)
    }

    pub fn approx_eq(&self, other: &Self) -> bool {
        self.x.approx_eq(&other.x) && self.y.approx_eq(&other.y)
    }

    pub fn dist(&self, p: Point) -> f64 {
        (*self - p).len()
    }

    fn len(&self) -> f64 {
        self.x.hypot(self.y)
    }
}

impl Add for Point {
    type Output = Point;
    fn add(self, other: Self) -> Self::Output {
        Point::new(self.x + other.x, self.y + other.y)
    }
}

impl Sub for Point {
    type Output = Point;
    fn sub(self, other: Self) -> Self::Output {
        Point::new(self.x - other.x, self.y - other.y)
    }
}

impl Mul for Point {
    type Output = Point;
    fn mul(self, other: Self) -> Self::Output {
        let c0: Complex<f64> = self.into();
        let c1: Complex<f64> = other.into();
        Point::from(c0 * c1)
    }
}

impl Mul<f64> for Point {
    type Output = Point;
    fn mul(self, a: f64) -> Self::Output {
        Point::new(self.x * a, self.y * a)
    }
}

impl Div for Point {
    type Output = Point;
    fn div(self, other: Self) -> Self::Output {
        let c0: Complex<f64> = self.into();
        let c1: Complex<f64> = other.into();
        Point::from(c0 / c1)
    }
}

impl Div<f64> for Point {
    type Output = Point;
    fn div(self, a: f64) -> Self::Output {
        Point::new(self.x / a, self.y / a)
    }
}

impl From<Complex<f64>> for Point {
    fn from(c: Complex<f64>) -> Point {
        Point::new(c.re, c.im)
    }
}

impl Into<Complex<f64>> for Point {
    fn into(self) -> Complex<f64> {
        Complex::new(self.x, self.y)
    }
}

impl FromStr for Point {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn rational_str_to_f64(s: &str) -> f64 {
            let v: Vec<&str> = s.split_terminator('/').collect::<Vec<&str>>();
            if v.len() == 1 {
                if let Some(f) = BigInt::parse_bytes(s.as_bytes(), 10).unwrap().to_f64() {
                    f
                } else {
                    warn!("Can not convert to f64: {}", s);
                    0.0
                }
                // BigInt::parse_bytes(s.as_bytes(), 10).unwrap().to_f64().unwrap_or({
                //     warn!("Can not convert to f64: {}", s);
                //     0.0
                // })
            } else {
                assert_eq!(v.len(), 2);
                let r = BigRational::new(v[0].parse().unwrap(), v[1].parse().unwrap());
                bigrational_to_f64(r)
            }
        }

        let v = s.split_terminator(',').collect::<Vec<&str>>();
        assert_eq!(v.len(), 2);
        Ok(Point::new(rational_str_to_f64(v[0]), rational_str_to_f64(v[1])))
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{},{}", self.x, self.y)
    }
}

// a -> b -> c
pub fn almost_rightturn(a: Point, b: Point, c: Point) -> bool {
    let r = (b - a).cross(c - b);
    debug!("rightTurn: a: {}, b: {}, c: {} => r: {}", a, b, c, r);
    // r < 0.0
    r.approx_eq(&0.0) || r < 0.0
}

#[test]
fn cross_test() {
    assert_eq!(Point::new(1.0, 0.0).cross(Point::new(0.0, 1.0)), 1.0);
    assert_eq!(Point::new(1.0, 0.0).cross(Point::new(1.0, 0.0)), 0.0);
    assert_eq!(Point::new(1.0, 0.0).cross(Point::new(0.0, -1.0)), -1.0);
    assert_eq!(Point::new(1.0, 0.0).cross(Point::new(-1.0, 0.0)), 0.0);
}

#[test]
fn bigrational_test() {
    let a = BigRational::from_float(1.0 / 3.0).unwrap();
    assert_eq!(&a.to_string(), "6004799503160661/18014398509481984");

    // let a = BigRational::new(BigInt::from_usize(1).unwrap(), BigInt::from_usize(3).unwrap());
    // assert_eq!(bigrational_to_f64(a), 0.3);
}

pub fn bigrational_to_f64(r: BigRational) -> f64 {
    if let Some(n) = r.numer().to_f64() {
        if let Some(d) = r.denom().to_f64() {
            n / d
        } else {
            warn!("Can not convert bigrational to f64: {}", r);
            warn!("r.denom(): {:?}", r.denom().to_f64());
            0.0
        }
    } else {
        warn!("Can not convert bigrational to f64: {}", r);
        warn!("r.numer(): {:?}", r.numer().to_f64());
        0.0
    }
    // TODO: Why this does not work???
    // r.numer().to_f64().and_then(|f1| {
    //     r.denom().to_f64().and_then(|f2| Some(f1 / f2))
    // }).unwrap_or({
    //     warn!("Can not convert bigrational to f64: {}", r);
    //     warn!("r.numer(): {:?}", r.numer().to_f64());
    //     warn!("r.denom(): {:?}", r.denom().to_f64());
    //     0.0
    // })
}

#[test]
fn point_display() {
    assert_eq!(&Point::new(1.0, 1.0).to_string(), "1,1");
    assert_eq!(&Point::new(1.1, 1.1).to_string(), "1.1,1.1");
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Polygon(pub Vec<Point>);

impl Index<usize> for Polygon {
    type Output = Point;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

pub struct Transform {
    parallel_move: Point,
    cos: f64,
    sin: f64,
    cos_r: f64,
    sin_r: f64,
}

impl Transform {
    pub fn new(parallel_move: Point, rotate: f64) -> Transform {
        Transform {
            parallel_move: parallel_move,
            cos: rotate.cos(),
            sin: rotate.sin(),
            cos_r: (-rotate).cos(),
            sin_r: (-rotate).sin(),
        }
    }

    pub fn from_line(m: Line) -> Transform {
        let v = m.b - m.a;
        Transform::new(Point::new(-m.a.x, -m.a.y), -(v.y.atan2(v.x)))
    }

    pub fn apply(&self, p: Point) -> Point {
        let p = p + self.parallel_move;
        let p = Point::new(self.cos * p.x - self.sin * p.y,
                           self.sin * p.x + self.cos * p.y);
        p
    }

    pub fn unapply(&self, p: Point) -> Point {
        let p = Point::new(self.cos_r * p.x - self.sin_r * p.y,
                           self.sin_r * p.x + self.cos_r * p.y);
        let p = p - self.parallel_move;
        p
    }
}

impl Polygon {
    pub fn new(ps: Vec<Point>) -> Polygon {
        // TODO: Assert convex
        Polygon(ps)
    }

    pub fn is_positive(&self) -> bool {
        let seg0 = self[1] - self[0];
        let seg1 = self[2] - self[1];
        seg0.x * seg1.y - seg0.y * seg1.x > 0.0
    }

    fn edges(&self) -> Vec<(Point, Point)> {
        (0..self.len()).map(|i| (self[i], self[(i + 1) % self.len()])).collect()
    }

    fn contains(&self, q: &Polygon) -> bool {
        q.iter().all(|p| self.contains_point(*p))
    }

    fn contains_polygon(&self, q: &Polygon) -> bool {
        unimplemented!();
    }
}

impl Deref for Polygon {
    type Target = [Point];
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct Line {
    pub a: Point,
    pub b: Point,
}

impl Line {
    pub fn new(a: Point, b: Point) -> Self {
        Line { a: a, b: b }
    }
}

#[derive(Clone, Copy)]
struct LineVec(Point);

impl LineVec {
    fn as_point(&self) -> Point {
        self.0
    }

    fn new(a: Point, b: Point) -> Self {
        LineVec(b - a)
    }

    fn colinear(&self, n: LineVec) -> bool {
        self.cross(n).approx_eq(&0.0)
    }

    fn cross(&self, n: LineVec) -> f64 {
        self.0.cross(n.0)
    }
}

fn approx_between(x: f64, a: f64, b: f64) -> bool {
    x.approx_eq(&a) || x.approx_eq(&b) || ((a < x) && (x < b))
}

impl Line {
    fn vec(&self) -> LineVec {
        LineVec(self.b - self.a)
    }

    fn intersect(&self, n: Line) -> Option<Point> {
        let u = self.vec();
        let v = n.vec();
        if u.colinear(v) {
            None
        } else {
            let d1: f64 = LineVec::new(self.a, n.a).cross(u);
            let d2: f64 = LineVec::new(self.a, n.b).cross(u);
            Some(LineVec::new(n.a * d2, n.b * d1).as_point() / (d1 - d2))
        }
    }

    pub fn intersect_with_edge(&self, e: Edge) -> Option<Point> {
        self.intersect(e.into())
            .and_then(|p| {
                if e.contains(p) {
                    Some(p)
                } else {
                    None
                }
            })
    }

    pub fn reflect(&self, p: Point) -> Point {
        let z = p - self.a;
        let w = self.vec().as_point();
        (z / w).conjugate() * w + self.a
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct Edge {
    pub a: Point,
    pub b: Point,
}

impl From<Line> for Edge {
    fn from(m: Line) -> Edge {
        Edge::new(m.a, m.b)
    }
}

impl From<Edge> for Line {
    fn from(e: Edge) -> Line {
        Line::new(e.a, e.b)
    }
}

impl Edge {
    pub fn new(a: Point, b: Point) -> Self {
        Edge { a: a, b: b }
    }

    fn contains(&self, p: Point) -> bool {
        approx_between(p.x, self.a.x.min(self.b.x), self.a.x.max(self.b.x)) &&
        approx_between(p.y, self.a.y.min(self.b.y), self.a.y.max(self.b.y))
    }

    pub fn intersect(&self, e: Edge) -> Option<Point> {
        Line::from(*self)
            .intersect(Line::from(e))
            .and_then(|p| {
                if self.contains(p) && e.contains(p) {
                    Some(p)
                } else {
                    None
                }
            })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct SourcePoint {
    pub p: Point,
    pub index: usize,
}

impl SourcePoint {
    pub fn new(p: Point, index: usize) -> SourcePoint {
        SourcePoint {
            p: p,
            index: index,
        }
    }

    pub fn reflect_on(self, m: Line) -> SourcePoint {
        SourcePoint::new(m.reflect(self.p), self.index)
    }
}

pub trait Points {
    fn points(&self) -> Vec<Point>;

    fn edges(&self) -> Vec<Edge> {
        let ps = self.points();
        (0..ps.len()).map(|i| Edge::new(ps[i], ps[(i + 1) % ps.len()])).collect()
    }

    fn area(&self) -> f64 {
        let p = self.points();
        let mut area = 0.0;
        for i in 0..p.len() {
            area += p[i].cross(p[(i + 1) % p.len()])
        }
        (area / 2.0).abs()
    }

    fn contains_polygon<T: Points>(&self, q: &T) -> bool {
        q.points().iter().all(|p| self.contains_point(*p))
    }

    fn contains_point(&self, p: Point) -> bool {
        let mut cnt = 0;
        let mut pre = 0;
        let mut same_result = 0;
        while same_result < 3 {
            cnt = 0;
            let far = Point::new((100000.0 + 100.0 * rand::random::<f64>()),
                                 100000.0 + 100.0 * rand::random::<f64>());
            for e in self.edges() {
                // if (zero(pointToSegmentDist(a, b, p))) return true; // Boundary Case
                if e.intersect(Edge::new(p, far)).is_some() {
                    cnt = cnt + 1;
                }
            }
            if cnt == pre {
                same_result += 1
            } else {
                same_result = 0;
            }
            pre = cnt;
        }
        return cnt % 2 == 1;
    }
}

impl Points for Facet {
    fn points(&self) -> Vec<Point> {
        self.iter().map(|sp| sp.p).collect()
    }
}

impl Points for Polygon {
    fn points(&self) -> Vec<Point> {
        self.0.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Facet(Vec<SourcePoint>);

impl Deref for Facet {
    type Target = [SourcePoint];
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl From<Vec<SourcePoint>> for Facet {
    fn from(m: Vec<SourcePoint>) -> Self {
        Facet(m)
    }
}

impl From<Facet> for Polygon {
    fn from(m: Facet) -> Polygon {
        Polygon::new(m.into_iter().map(|sp| sp.p).collect())
    }
}

impl Facet {
    pub fn from_points(points: Vec<(f64, f64)>, start: usize) -> Facet {
        (start..)
            .zip(points.into_iter())
            .map(|(i, (x, y))| SourcePoint::new(Point::new(x, y), i))
            .collect::<Vec<_>>()
            .into()
    }

    pub fn from_polygon(p: &Polygon, start: usize) -> Facet {
        (start..)
            .zip(&p.0)
            .map(|(i, &p)| SourcePoint::new(p, i))
            .collect::<Vec<_>>()
            .into()
    }

    pub fn new_square() -> Facet {
        Facet::from_points(vec![
            (0.0, 0.0),
            (1.0, 0.0),
            (1.0, 1.0),
            (0.0, 1.0),
        ],
                    0)
    }

    pub fn reflect_on(&self, m: Line) -> Facet {
        let mut reflected: Facet =
            self.iter().map(|sp| sp.reflect_on(m)).collect::<Vec<_>>().into();
        // Make it counter-clockwise
        reflected.0.reverse();
        reflected
    }

    pub fn cut(&self, m: Line, sources: usize) -> CutResult {
        let mut sources = sources;
        let mut left = vec![];
        let mut right = vec![];
        let mut current = {
            if (m.a - self[0].p).cross(m.b - m.a) > 0.0 {
                0  // left size of line
            } else {
                1  // right size of line
            }
        };
        for i in 0..self.len() {
            let j = if i == self.len() - 1 {
                0
            } else {
                i + 1
            };
            let a = self[i];
            let b = self[j];
            if current == 0 {
                left.push(a);
            } else {
                right.push(a);
            }
            if let Some(p) = m.intersect_with_edge(Edge::new(a.p, b.p)) {
                // TODO:
                current = 1 - current;
                let sp = SourcePoint::new(p, sources);
                sources += 1;
                left.push(sp);
                right.push(sp);
            }
        }
        CutResult {
            left: left.into(),
            right: right.into(),
            sources: sources,
        }
    }

    fn edges(&self) -> Vec<(SourcePoint, SourcePoint)> {
        (0..self.len()).map(|i| (self[i], self[(i + 1) % self.len()])).collect()
    }

    fn bottomleft(&self) -> SourcePoint {
        let minx = self.iter().min_by_key(|sp| OrderedFloat(sp.p.x)).unwrap();
        let miny = self.iter().min_by_key(|sp| OrderedFloat(sp.p.y)).unwrap();

        let base = Point::new(minx.p.x, miny.p.y);

        let start = self[0];
        self.iter()
            .skip(1)
            .fold((base.dist(start.p), start), |(mindist, selected), sp| {
                let d = base.dist(sp.p);
                if d < mindist {
                    (d, *sp)
                } else {
                    (mindist, selected)
                }
            })
            .1
    }

    pub fn convex_hull(&self) -> Facet {
        let base = self.bottomleft();
        let mut sps = self.iter().cloned().filter(|sp| sp.index != base.index).collect::<Vec<_>>();

        sps.sort_by(|sp1, sp2| {
            let cross: f64 = (sp2.p - base.p).cross(sp1.p - base.p);
            if cross.approx_eq(&0.0) {
                Ordering::Equal
            } else if cross > 0.0 {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        });
        debug!("sps: {:?}", sps);
        let mut stack = vec![base];
        for sp in sps {
            stack.push(sp);
            while stack.len() >= 3 &&
                  almost_rightturn(stack[stack.len() - 3].p,
                                   stack[stack.len() - 2].p,
                                   stack[stack.len() - 1].p) {
                debug!("remove: {:?}: from {:?}", stack[stack.len() - 2], stack);
                let len = stack.len();
                stack.remove(len - 2);
            }
        }
        stack.into()
    }

    fn most_clockwise_next_from(&self, prev: Point, current: Point) -> SourcePoint {
        let v0 = current - prev;
        self.0
            .iter()
            .min_by_key(|sp| {
                let v1 = sp.p - current;
                OrderedFloat(v0.cross(v1))
            })
            .unwrap()
            .clone()
    }

    // http://stackoverflow.com/questions/2667748/how-do-i-combine-complex-polygons
    pub fn union(&self, q: &Facet, sources: usize) -> Option<(Facet, usize)> {
        let mut sources = sources;

        let edges0 = self.edges();
        let edges1 = q.edges();

        let mut ip0: HashMap<usize, Vec<SourcePoint>> = HashMap::new();
        let mut ip1: HashMap<usize, Vec<SourcePoint>> = HashMap::new();

        for &(ea, eb) in &edges0 {
            for &(fa, fb) in &edges1 {
                if let Some(p) = Edge::new(ea.p, eb.p).intersect(Edge::new(fa.p, fb.p)) {
                    ip0.entry(ea.index).or_insert(vec![]).push(SourcePoint::new(p, sources));
                    ip1.entry(fa.index).or_insert(vec![]).push(SourcePoint::new(p, sources));
                    sources += 1;
                }
            }
        }
        if ip0.is_empty() && ip1.is_empty() {
            if self.contains_polygon(q) {
                Some((self.clone(), sources))
            } else if q.contains_polygon(self) {
                Some((q.clone(), sources))
            } else {
                None
            }
        } else {
            type Adj = (SourcePoint, Vec<SourcePoint>);

            let mut adj: BTreeMap<usize, Adj> = BTreeMap::new();

            fn build_graph(edges: &Vec<(SourcePoint, SourcePoint)>,
                           ip: &HashMap<usize, Vec<SourcePoint>>,
                           adj: &mut BTreeMap<usize, Adj>) {
                fn connect(a: SourcePoint, b: SourcePoint, adj: &mut BTreeMap<usize, Adj>) {
                    adj.entry(a.index).or_insert((a, vec![])).1.push(b);
                    adj.entry(b.index).or_insert((b, vec![])).1.push(a);
                }

                for &(a, b) in edges {
                    match ip.get(&a.index) {
                        Some(v) => {
                            let mut v = v.clone();
                            v.sort_by_key(|sp| OrderedFloat(a.p.dist(sp.p)));
                            connect(a, v[0], adj);
                            for i in 0..(v.len() - 1) {
                                connect(v[i], v[i + 1], adj);
                            }
                            connect(v[v.len() - 1], b, adj);
                        }
                        None => {
                            connect(a, b, adj);
                        }
                    }
                }
            }

            build_graph(&edges0, &ip0, &mut adj);
            build_graph(&edges1, &ip1, &mut adj);
            println!("adj: {:?}", serde_json::to_string_pretty(&adj));

            let sps: Facet = adj.values().map(|e| e.0).collect::<Vec<SourcePoint>>().into();
            let start = sps.bottomleft();

            println!("start: {:?}", start);

            let mut visited: HashSet<usize> = HashSet::new();

            let mut answer = Vec::new();
            answer.push(start);

            let &(a, ref bs) = adj.get(&start.index).unwrap();
            assert!(!bs.is_empty());

            let mut prev = start;
            let mut current = Facet::from(bs.clone())
                .most_clockwise_next_from(start.p - Point::new(1.0, 0.0), start.p);
            // Prevent the first iteration from visiting start.
            visited.insert(start.index);
            while start.index != current.index {
                println!("current: {:?}", current);
                visited.insert(current.index);
                answer.push(current);
                let &(a, ref bs) = adj.get(&current.index).unwrap();
                let bs =
                    bs.iter().cloned().filter(|b| !visited.contains(&b.index)).collect::<Vec<_>>();
                println!("next bs: {:?}", bs);
                assert!(!bs.is_empty());
                let next = Facet::from(bs.clone()).most_clockwise_next_from(prev.p, current.p);
                println!("next: {:?}", next);
                prev = current;
                current = next;
                visited.remove(&start.index);
            }
            Some((answer.into(), sources))
        }
    }

    pub fn intersection(&self, q: &Facet, sources: usize) -> (Facet, usize) {
        let mut sources = sources;

        let edges0 = self.edges();
        let edges1 = q.edges();

        let mut ip = vec![];

        for &(ea, eb) in &edges0 {
            for &(fa, fb) in &edges1 {
                if let Some(p) = Edge::new(ea.p, eb.p).intersect(Edge::new(fa.p, fb.p)) {
                    ip.push(SourcePoint::new(p, sources));
                    sources += 1;
                }
            }
        }
        for sp in self.iter() {
            if q.contains_point(sp.p) {
                ip.push(*sp);
            }
        }
        for sp in q.iter() {
            if self.contains_point(sp.p) {
                ip.push(*sp);
            }
        }
        (Facet::from(ip).convex_hull(), sources)
    }
}

pub struct CutResult {
    pub left: Facet,
    pub right: Facet,
    pub sources: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_point_eq(a: Point, b: Point) {
        assert!(a.approx_eq(&b))
    }

    #[test]
    fn line_test() {
        let m = Line::new(Point::new(0.5, 0.0), Point::new(0.5, 1.0));
        let n = Line::new(Point::new(0.0, 0.5), Point::new(1.0, 0.5));
        assert_point_eq(m.intersect(n).unwrap(), Point::new(0.5, 0.5));

        let m = Line::new(Point::new(0.5, 0.0), Point::new(0.5, 1.0));
        let n = Line::new(Point::new(0.0, 0.5), Point::new(0.1, 0.5));
        assert_point_eq(m.intersect(n).unwrap(), Point::new(0.5, 0.5));

        // segment
        assert_point_eq(m.intersect_with_edge(Edge::new(Point::new(0.0, 0.5),
                                                           Point::new(1.0, 0.5)))
                            .unwrap(),
                        Point::new(0.5, 0.5));

        assert!(m.intersect_with_edge(Edge::new(Point::new(0.0, 0.5), Point::new(0.1, 0.5)))
            .is_none());

        assert_point_eq(m.reflect(Point::new(0.0, 0.5)), Point::new(1.0, 0.5));
        assert_point_eq(m.reflect(Point::new(0.4, 2.0)), Point::new(0.6, 2.0));
    }

    #[test]
    fn transform_test() {
        let a = Point::new(1.0, 0.0);
        let b = Point::new(2.0, 1.0);
        let m = Line::new(a, b);
        let t = Transform::from_line(m);

        let a1 = t.apply(a);
        let b1 = t.apply(b);
        assert_point_eq(a1, Point::new(0.0, 0.0));
        assert_point_eq(b1, Point::new(2.0f64.sqrt(), 0.0));

        let a2 = t.unapply(a1);
        let b2 = t.unapply(b1);
        assert_point_eq(a2, a);
        assert_point_eq(b2, b)
    }

    #[test]
    fn facet_test_convex() {
        let facet = Facet::from_points(vec![
            (0.0, 0.0),
            (1.0, 0.0),
            (1.0, 1.0),
            (0.0, 1.0),
            (0.5, 0.5),
            // (20.0, 10.0),
            (10.0, 10.0),
        ],
                                       0);
        let convex = facet.convex_hull();
        assert_eq!(convex.iter().map(|sp| sp.index).collect::<Vec<_>>(),
                   vec![0, 1, 5, 3]);
    }

    #[test]
    fn facet_union_test() {
        let facet1 = Facet::new_square();

        let ps = vec![(0.5, 0.5), (1.5, 0.5), (1.5, 1.5), (0.5, 1.5)];
        let facet2 = Facet::from_points(ps, 4);

        let (facet, sources) = facet1.union(&facet2, 8).unwrap();
        assert_eq!(sources, 10);
        assert_eq!(facet.iter().map(|sp| sp.index).collect::<Vec<_>>(),
                   vec![0, 1, 8, 5, 6, 7, 9, 3]);

        assert_eq!(facet.area(), 1.75);
    }

    #[test]
    fn facet_intersectionn_test() {
        let facet1 = Facet::new_square();

        let ps = vec![(0.5, 0.5), (1.5, 0.5), (1.5, 1.5), (0.5, 1.5)];
        let facet2 = Facet::from_points(ps, 4);

        let (facet, sources) = facet1.intersection(&facet2, 8);
        assert_eq!(sources, 10);
        assert_eq!(facet.len(), 4);
        assert_eq!(facet.iter().map(|sp| sp.index).collect::<Vec<_>>(),
                   vec![4, 8, 2, 9]);

        assert_eq!(facet.area(), 0.25);
    }

    #[test]
    fn facet_cut_test() {
        let facet = Facet::new_square();
        let c0 = Point::new(0.5, 0.0);
        let c1 = Point::new(0.5, 1.0);

        let CutResult { left, right, sources } =
            facet.cut(Line::new(Point::new(0.5, 0.0), Point::new(0.5, 1.0)), 4);

        assert_eq!(sources, 6);

        assert_eq!(left.len(), 4);
        assert_eq!(left[0].index, 0);
        assert_eq!(left[1].index, 4);
        assert_eq!(left[2].index, 5);
        assert_eq!(left[3].index, 3);

        assert_eq!(right.len(), 4);
        assert_eq!(right[0].index, 4);
        assert_eq!(right[1].index, 1);
        assert_eq!(right[2].index, 2);
        assert_eq!(right[3].index, 5);

        let CutResult { left, right, sources } =
            facet.cut(Line::new(Point::new(0.5, 2.0), Point::new(0.5, 1.0)), 4);

        assert_eq!(sources, 6);

        assert_eq!(right.len(), 4);
        assert_eq!(right[0].index, 0);
        assert_eq!(right[1].index, 4);
        assert_eq!(right[2].index, 5);
        assert_eq!(right[3].index, 3);

        assert_eq!(left.len(), 4);
        assert_eq!(left[0].index, 4);
        assert_eq!(left[1].index, 1);
        assert_eq!(left[2].index, 2);
        assert_eq!(left[3].index, 5);
    }

    #[test]
    fn rightrurn_test() {
        let p0 = Point::new(0.0, 0.0);
        let p1 = Point::new(1.0, 0.0);
        let p2 = Point::new(2.0, 0.0);
        assert_eq!(almost_rightturn(p0, p2, p1), true);
        assert_eq!(almost_rightturn(p0, p1, p2), true);
    }

}
