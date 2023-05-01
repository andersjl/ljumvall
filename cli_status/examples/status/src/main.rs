use clap::{Arg, Command};
use ljumvall_cli_status::*;
use ljumvall_error::define_error;
use std::thread::sleep;
use std::time::Duration;

define_error!();

fn main() {
    let cmd = Command::new("test")
        .author("testauthor, <testauthor@example.com>")
        .version("4.7.11")
        .about("for testing")
        .arg(Arg::new("tested"))
        .arg(Arg::new("arg").long("arg"))
        .arg(Arg::new("error").long("error"));
    let input = cmd.clone().get_matches();
    let mut status = Status::new(cmd);
    status.set_min_period(Duration::from_millis(10));
    match input.get_one::<String>("tested").unwrap().as_str() {
        "note" => {
            status.note("first line", 0);
            if let Some(arg) = input.get_one::<String>("arg") {
                match arg.as_str() {
                    "quiet" => {
                        status.note(arg, 1);
                        status.set_verbosity(-1);
                        status.note(arg, 0);
                        status.note(arg, -2);
                    }
                    _ => {
                        status.note(arg, 0);
                    }
                }
            } else {
                std::process::exit(110);
            }
        }
        "puts_clever" => {
            status.puts_clever(Some("first line"));
            if let Some(arg) = input.get_one::<String>("arg") {
                match arg.as_str() {
                    "iff dirty" => {
                        status.puts_clever(None);
                        status.puts_clever(Some(arg));
                    }
                    _ => {
                        std::process::exit(120);
                    }
                }
            } else {
                std::process::exit(121);
            }
        }
        "tell" => {
            // set last time to now
            assert!(status.tell(Some("first line")));
            // to soon
            sleep(Duration::from_millis(9));
            assert!(!status.tell(Some("not time")));
            // time is up
            sleep(Duration::from_millis(2));
            // no tale
            assert!(!status.tell(None));
            // time is up
            sleep(Duration::from_millis(10));
            // tale
            assert!(status.tell(Some("tale")));
        }
        "usage" => status.usage(
            input
                .get_one::<String>("error")
                .map(|e| Error::new(&e))
                .as_ref(),
        ),
        "unwrap_or_usage" => {
            let error = input.get_one::<String>("error");
            assert!(error.is_some());
            let result: Result<i32, Error> = Err(Error::new(error.unwrap()));
            status.unwrap_or_usage(result);
        }
        _ => std::process::exit(42),
    }
}
