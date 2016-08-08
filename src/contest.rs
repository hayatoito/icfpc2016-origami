use serde_json;

use super::netio;
use super::fileio;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct SnapshotList {
    ok: bool,
    snapshots: Vec<SnapShot>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct SnapShot {
    snapshot_hash: String,
    snapshot_time: usize,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ContestStatus {
    problems: Vec<Problem>,
    snapshot_time: usize,
    leaderboard: Vec<Leaderboard>,
    users: Vec<User>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Problem {
    ranking: Vec<Ranking>,
    publish_time: usize,
    solution_size: usize,
    problem_id: usize,
    owner: String,
    problem_size: usize,
    problem_spec_hash: String,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Ranking {
    resemblance: f64,
    solution_size: usize
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Leaderboard {
    username: String,
    score: f64,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct User {
    username: String,
    display_name: String,
}

pub fn hello() -> String {
    netio::read("http://2016sv.icfpcontest.org/api/hello")
}

fn update_snapshot_list() -> SnapshotList {
    // let json = netio::read("http://2016sv.icfpcontest.org/api/snapshot/list");
    let json = netio::read("http://130.211.240.134/api/snapshot/list");
    fileio::write_json(&json, "snapshot.json");
    let s: SnapshotList = serde_json::from_str(&json).unwrap();
    s
}

fn load_snapshot_list(path: &str) -> SnapshotList {
    let s = fileio::read(path);
    let s: SnapshotList = serde_json::from_str(&s).unwrap();
    s
}

fn latest_snapshot_hash(snapshot_list: SnapshotList) -> String {
    let mut snapshots = snapshot_list.snapshots;
    snapshots.sort_by_key(|s| -(s.snapshot_time as isize));
    snapshots[0].snapshot_hash.clone()
}

fn load_status(path: &str) -> ContestStatus {
    let s = fileio::read(path);
    let s: ContestStatus = serde_json::from_str(&s).unwrap();
    s
}

pub fn update_problmes() {
    let snapshot_list = update_snapshot_list();
    let hash = latest_snapshot_hash(snapshot_list);
    let json = netio::lookup(&hash);
    fileio::write_json(&json, "status.json");
    let status: ContestStatus = serde_json::from_str(&json).unwrap();
    debug!("status: {:?}", status);

    for p in &status.problems {
        println!("problem info: {:?}", p);
        if fileio::problem_spec_exists(p.problem_id) {
            println!("problem {} exists. Skipping", p.problem_id);
        } else {
            let txt = netio::lookup(&p.problem_spec_hash);
            println!("problem txt: {:?}", txt);
            fileio::write_problem_spec(&txt, p.problem_id);
        }
    }
}
