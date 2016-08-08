#![feature(question_mark)]
#![feature(custom_derive, plugin)]

#![plugin(serde_macros)]

#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]

#[macro_use]
extern crate log;
extern crate loggerv;

#[macro_use]
extern crate clap;
extern crate chrono;
extern crate num;
extern crate regex;
extern crate serde;
extern crate serde_json;
extern crate euclid;
extern crate ordered_float;
extern crate rand;

use clap::{Arg, App, SubCommand};

mod contest;
mod fileio;
mod netio;
mod prelude;
mod problem;
mod solution;
mod solver;

fn main() {
    let matches = App::new("my-origami")
        .version("1.0")
        .arg(Arg::with_name("v")
            .short("v")
            .multiple(true)
            .help("Sets the level of verbosity"))
        .subcommand(SubCommand::with_name("hello"))
        .subcommand(SubCommand::with_name("lookup"))
        .subcommand(SubCommand::with_name("update_problems"))
        .subcommand(SubCommand::with_name("update_problems_json"))
        // .subcommand(SubCommand::with_name("show_problem")
        //             .arg(Arg::with_name("id")
        //                  .required(true)))
        .subcommand(SubCommand::with_name("run_origami"))
        .subcommand(SubCommand::with_name("submit_solution")
                    .arg(Arg::with_name("id")
                         .required(true))
                    .arg(Arg::with_name("file")
                         .required(true)))
        .subcommand(SubCommand::with_name("show_simple_problems"))
        .subcommand(SubCommand::with_name("run_simple_solver"))
        .get_matches();

    loggerv::init_with_verbosity(matches.occurrences_of("v")).unwrap();

    if let Some(_) = matches.subcommand_matches("hello") {
        println!("res: {}", contest::hello());
    } else if let Some(matches) = matches.subcommand_matches("update_problems") {
        contest::update_problmes()
    } else if let Some(matches) = matches.subcommand_matches("update_problems_json") {
        problem::update_problems_json();
    } else if let Some(matches) = matches.subcommand_matches("run_origami") {
        let (origami, origami_unfold) = solver::run_origami();

        println!("origami: {:?}", origami);
        let json = serde_json::to_string_pretty(&origami).unwrap();
        println!("origami json: {}", json);
        fileio::write_json(&json, "origami.json");

        println!("origami_unfold: {:?}", origami_unfold);
        let json = serde_json::to_string_pretty(&origami_unfold).unwrap();
        println!("origami_unfold json: {}", json);
        fileio::write_json(&json, "origami-unfold.json");
    } else if let Some(matches) = matches.subcommand_matches("submit_solution") {
        let id: usize = matches.value_of("id").unwrap().parse().unwrap();
        let file = matches.value_of("file").unwrap();
        solution::submit(id, file);
    } else if let Some(matches) = matches.subcommand_matches("show_simple_problems") {
        problem::show_simple_problems()
    } else if let Some(matches) = matches.subcommand_matches("run_simple_solver") {
        solver::run_simple_solver();
    } else {
        error!("No subcommand");
    }
    println!("Hello, Origami!");
}
