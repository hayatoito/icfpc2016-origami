use std::io::prelude::*;
use std::fs;
use chrono;
use chrono::*;

use std;
use std::io::BufReader;
use std::path::Path;
use std::collections;

pub fn read(path: &str) -> String {
    let f = fs::File::open(path).unwrap();
    let mut f = BufReader::new(f);
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    s
}

pub fn write(path: &str, content: &str) {
    let mut f = fs::File::create(path).unwrap();
    f.write(content.as_bytes()).unwrap();
}

fn now() -> String {
    let dt = chrono::Local::now();
    format!("{}{:02}{:02}{:2}{:2}{:2}",
            dt.year(),
            dt.month(),
            dt.day(),
            dt.hour(),
            dt.minute(),
            dt.second())
}

pub fn write_problem_spec(content: &str, id: usize) {
    let path = format!("{}/problems/problem-{:04}.txt",
                       env!("CARGO_MANIFEST_DIR"),
                       id);
    write(&path, content);
}

pub fn write_solution_spec(content: &str, id: usize) {
    let path = format!("{}/solutions/solution-{:04}-spec.txt",
                       env!("CARGO_MANIFEST_DIR"),
                       id);
    write(&path, content);
}

pub fn write_solution_spec_generated(content: &str, id: usize) {
    let path = format!("{}/solutions/solution-{:04}-spec-generated.txt",
                       env!("CARGO_MANIFEST_DIR"),
                       id);
    write(&path, content);
}

pub fn path_solution_spec_generated(id: usize) -> String {
    format!("{}/solutions/solution-{:04}-spec-generated.txt",
            env!("CARGO_MANIFEST_DIR"),
            id)
}

pub fn write_solution_response(content: &str, id: usize) {
    let path = format!("{}/solutions/solution-{:04}-response.json",
                       env!("CARGO_MANIFEST_DIR"),
                       id);
    write(&path, content);
}

pub fn read_problem_spec(id: usize) -> String {
    let path = format!("{}/problems/problem-{:04}.txt",
                       env!("CARGO_MANIFEST_DIR"),
                       id);
    read(&path)
}

pub fn read_solution_response(id: usize) -> Option<String> {
    let path = format!("{}/solutions/solution-{:04}-response.json",
                       env!("CARGO_MANIFEST_DIR"),
                       id);

    if Path::new(&path).exists() {
        Some(read(&path))
    } else {
        None
    }
}

pub fn problem_spec_exists(id: usize) -> bool {
    let path = format!("{}/problems/problem-{:04}.txt",
                       env!("CARGO_MANIFEST_DIR"),
                       id);
    Path::new(&path).exists()
}

pub fn write_json(content: &str, filename: &str) {
    let path = format!("{}/json/{}", env!("CARGO_MANIFEST_DIR"), filename);
    debug!("path: {}", path);
    write(&path, content);
}

pub struct Scanner<R: std::io::Read> {
    reader: BufReader<R>,
    buffer: collections::VecDeque<String>,
}

impl<R: std::io::Read> Scanner<R> {
    pub fn new(reader: BufReader<R>) -> Scanner<R> {
        Scanner {
            reader: reader,
            buffer: collections::VecDeque::new(),
        }
    }

    pub fn next<T: std::str::FromStr>(&mut self) -> T {

        if self.buffer.len() == 0 {
            let mut input = String::new();
            self.reader.read_line(&mut input).ok();
            for word in input.split_whitespace() {
                self.buffer.push_back(word.to_string())
            }
        }

        let front = self.buffer.pop_front().unwrap();
        front.parse::<T>().ok().unwrap()
    }
}
