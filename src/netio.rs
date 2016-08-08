use std::io::prelude::*;
use std::process::Command;

use std::str;
use std::thread;
use std::time;

// TODO(hayato): Extern it
// static KEY: &'static str = "84-192db86cfbc5c403a46efdbec067922f";

// For postmortem server
static KEY: &'static str = "1052-4fcc6c30ed299c374fa7a792f7bb3673";

pub fn read(endpoint: &str) -> String {
    thread::sleep(time::Duration::from_secs(1));
    let res = Command::new("curl")
        .arg("-vv")
        .arg("--compressed")
        .arg("-L")
        .arg("-H")
        .arg("Expect:")
        .arg("-H")
        .arg(format!("X-API-Key: {}", KEY))
        .arg(endpoint)
        .output()
        .unwrap();

    debug!("curl: stderr: {}", str::from_utf8(&res.stderr).unwrap());
    str::from_utf8(&res.stdout).unwrap().to_string()
}

pub fn submit_solution(id: usize, file: &str) -> String {
    thread::sleep(time::Duration::from_secs(1));
    let res = Command::new("curl")
        .arg("-vv")
        .arg("--compressed")
        .arg("-L")
        .arg("-H")
        .arg("Expect:")
        .arg("-H")
        .arg(format!("X-API-Key: {}", KEY))
        .arg("-F")
        .arg(format!("problem_id={}", id))
        .arg("-F")
        .arg(format!("solution_spec=@{}", file))
        // .arg("http://2016sv.icfpcontest.org/api/solution/submit")
        .arg("http://130.211.240.134/api/solution/submit")
        .output()
        .unwrap();

    debug!("curl: stderr: {}", str::from_utf8(&res.stderr).unwrap());
    str::from_utf8(&res.stdout).unwrap().to_string()
}


pub fn lookup(hash: &str) -> String {
    // read(&format!("http://2016sv.icfpcontest.org/api/blob/{}", hash))
    read(&format!("http://130.211.240.134/api/blob/{}", hash))
}
