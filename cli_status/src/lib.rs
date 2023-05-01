use clap::Command;
use std::cell::RefCell;
use std::io::{stderr, stdout, Write};
use std::process::exit;
use std::time::{Duration, SystemTime};

/// The status of a command line program.
///
pub struct Status<'a> {
    command: RefCell<Command>,
    // Something is written after the last newline
    dirty_line: bool,
    // true if error status
    error: bool,
    // As if time were discrete with this period
    min_period: Duration,
    // closures always to run before exiting
    todo_at_exit: Vec<Box<dyn Fn() + 'a>>,
    // Cooperative multitasking
    todo_periodically: Vec<Task<'a>>,
    // Last time something was output
    told: SystemTime,
    // zero normal, negative quiet, positive verbose
    verbosity: i32,
}

impl<'a> Status<'a> {
    /// Nothing output, nothing scheduled, normal verbosity.
    ///
    pub fn new(command: Command) -> Self {
        Self {
            command: RefCell::new(command),
            // to ensure a newline if the program starts on a dirty line
            dirty_line: true,
            error: false,
            min_period: Duration::from_secs(1),
            todo_at_exit: Vec::new(),
            todo_periodically: Vec::new(),
            told: SystemTime::UNIX_EPOCH,
            verbosity: 0,
        }
    }

    /// `todo` is executed approximately once every `period` seconds, provided
    /// [`tell`](#method.tell) is called several times every `period`. If
    /// `start` is given, `todo` is not executed before that.
    ///
    pub fn do_periodically(
        &mut self,
        todo: impl Fn() + 'a,
        period: Duration,
        start: Option<SystemTime>,
    ) {
        self.todo_periodically.push(Task {
            next_time: if let Some(start) = start {
                start
            } else {
                SystemTime::now()
            },
            period,
            todo: Box::new(todo),
        });
    }

    /// `puts_clever` if `verbosity` is at least `at_verbosity`
    ///
    pub fn note(&mut self, message: &str, at_verbosity: i32) {
        if self.verbosity >= at_verbosity {
            self.puts_clever(Some(message));
        }
    }

    /// If dirty line, make a new line, print `s` if `Some`.
    ///
    pub fn puts_clever(&mut self, s: Option<&str>) {
        if self.dirty_line {
            self.print("\n");
            self.dirty_line = false;
        }
        if let Some(s) = s {
            self.print(s);
            self.dirty_line = true;
        }
    }

    /// Set error status. There is no way to clear the error once set.
    ///
    pub fn set_error(&mut self) {
        self.error = true;
    }

    /// Set the minimum period for e.g. [`tell()`](#method.tell). Default 1 s.
    ///

    pub fn set_min_period(&mut self, min_period: Duration) {
        self.min_period = min_period;
    }

    /// Set verbosity level. Zero is default. Negative is quiet and will stop
    /// [`tell`ing](#method.tell).
    ///
    /// Returns the old verbosity.
    ///
    pub fn set_verbosity(&mut self, verbosity: i32) -> i32 {
        let old = self.verbosity;
        self.verbosity = verbosity;
        old
    }

    /// If less than the [minimum period](#method.set_min_period) since last
    /// time do nothing.
    ///
    /// If [verbosity](#method.set_verbosity) is non-negative and `tale` is
    /// `Some`, clear the line and print `tale`.
    ///
    /// Run [scheduled tasks](#method.do_periodically) if their time is up.
    ///
    /// Set last time to now.
    ///
    /// Return `true` iff `tale` was printed.
    ///
    pub fn tell(&mut self, tale: Option<&str>) -> bool {
        use unicode_segmentation::UnicodeSegmentation;

        let now = SystemTime::now();
        if {
            let d = now.duration_since(self.told);
            d.is_ok() && d.unwrap() < self.min_period
        } {
            return false;
        }
        let tale_printed = self.verbosity >= 0 && tale.is_some();
        if tale_printed {
            let mut t = tale.unwrap();
            let mut used_len = 0usize;
            let maxlen =
                termsize::get().map(|sz| sz.cols).unwrap_or(80) as usize - 2;
            if self.dirty_line {
                self.print("\r");
            }
            for (len, (ix, _)) in (0..=maxlen).zip(t.grapheme_indices(true)) {
                used_len = len;
                if len == maxlen {
                    t = t.get(0..ix).unwrap();
                }
            }
            self.print(t);
            for _ in 0..(maxlen - used_len) {
                self.print(" ");
            }
            self.flush();
            self.dirty_line = !t.is_empty();
        }
        for Task { next_time, period, todo }
            in self.todo_periodically.iter_mut()
        {
            if now >= *next_time {
                todo();
                *next_time =
                    (now + self.min_period).max(*next_time + *period);
            }
        }
        self.told = now;
        tale_printed
    }

    /// `Result::unwrap()` except [`usage()`](#method.usage) instead of
    /// `panic()`.
    ///
    pub fn unwrap_or_usage<T, E: std::error::Error>(
        &mut self,
        result: Result<T, E>,
    ) -> T {
        match result {
            Ok(good) => good,
            Err(e) => self.usage(Some(&e)),
        }
    }

    /// Exit the process after writing [`clap::Command::render_usage()`
    /// ](https://docs.rs/clap/latest/clap/struct.Command.html#method.render_usage)
    /// to `stdout`.
    ///
    /// If [`set_error()`](#method.set_error) or `err` is `Some`, write the
    /// error text (if `error` is `Some`) and a newline before the usage text
    /// to `stderr` and `exit 1`.
    ///
    pub fn usage(&mut self, err: Option<&impl std::error::Error>) -> ! {
        // TBD show status
        if let Some(e) = err {
            self.error = true;
            self.print(&format!("{}\n", e));
        }
        self.print(&format!(
            "{}\n",
            self.command.borrow_mut().render_usage()
        ));
        self.exit()
    }

    // --- private -----------------------------------------------------------

    fn exit(&self) -> ! {
        for todo in &self.todo_at_exit {
            todo();
        }
        exit(self.error as i32)
    }

    // flush the active output stream
    fn flush(&self) {
        let _ = if self.error {
            stderr().flush()
        } else {
            stdout().flush()
        };
    }

    /// `print!()` or `eprint!()` depending on error status.
    ///
    fn print(&self, s: &str) {
        if self.error {
            eprint!("{}", s);
        } else {
            print!("{}", s);
        }
    }
}

// a task that is scheduled for periodic execution.
struct Task<'a> {
    // the next time todo should be run
    next_time: SystemTime,
    period: Duration,
    // closure to run
    todo: Box<dyn Fn() + 'a>,
}

#[cfg(test)]
#[allow(unused_assignments)]
#[allow(unused_mut)]
#[allow(unused_variables)]
mod tests {
    use super::*;
    use clap::Arg;
    use ljumvall_test_utils::{test_crate, TestCrateOutput};
    use regex::Regex;
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::thread::sleep;

    fn command() -> Command {
        Command::new("test")
            .author("testauthor, <testauthor@example.com>")
            .version("4.7.11")
            .about("for testing")
            .arg(Arg::new("a_positional_argument"))
            .arg(Arg::new("an_option").long("an_option"))
    }

    fn get_log_count(s: &str) -> f64 {
        (Regex::new(r#"\d+ \| "#).unwrap().find_iter(s).count() - 1) as f64
    }

    fn get_log_first(s: &str) -> i64 {
        Regex::new(r#"^(\d+)"#)
            .unwrap()
            .captures(s)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .parse()
            .unwrap()
    }

    fn get_log_last(s: &str) -> i64 {
        Regex::new(r#"(\d+) \| $"#)
            .unwrap()
            .captures(s)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .parse()
            .unwrap()
    }

    fn get_log_period(s: &str) -> f64 {
        (get_log_last(s) - get_log_first(s)) as f64 / get_log_count(s)
    }

    fn now_millis() -> i64 {
        unix_millis(SystemTime::now())
    }

    fn unix_millis(t: SystemTime) -> i64 {
        t.duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis() as i64
    }

    speculate2::speculate! {

        describe "struct Status" {
            before {
                let mut arg: Option<&str> = None;
                let mut error: Option<&str> = None;
                let mut output = TestCrateOutput::Whatever;
                let mut now: i64 = 0;
                // 17 ms period start now
                let log1 = Rc::new(RefCell::new(String::new()));
                // 17 ms period start after 11 ms
                let log2 = Rc::new(RefCell::new(String::new()));
                // 7 ms period (impossible!) start now
                let log3 = Rc::new(RefCell::new(String::new()));
                let mut st = Status::new(command());
                st.set_min_period(Duration::from_millis(10));
                let mut use_test_crate = false;
                let mut tested = "";
            }

            context "do_periodically()" {
                before {
                    now = now_millis();
                    // 17 ms period start now
                    st.do_periodically(
                        || {
                            *log1.borrow_mut()
                                += &format!("{} | ", now_millis() - now);
                        },
                        Duration::from_millis(17),
                        None,
                    );
                    // 17 ms period start after 11 ms
                    st.do_periodically(
                        || {
                            *log2.borrow_mut()
                                += &format!("{} | ", now_millis() - now);
                        },
                        Duration::from_millis(17),
                        Some(SystemTime::now() + Duration::from_millis(11)),
                    );
                    // 7 ms period (impossible!) start now
                    st.do_periodically(
                        || {
                            *log3.borrow_mut()
                                += &format!("{} | ", now_millis() - now);
                        },
                        Duration::from_millis(7),
                        None,
                    );
                    for _ in 1..=200 {
                        sleep(Duration::from_millis(1));
                        st.tell(None);
                    }
                }

                it "starts without delay" {
                    let log = log1.borrow().as_str().to_string();
                    assert!(get_log_first(&log) < 3);
                }

                it "is executed with period" {
                    let log = log1.borrow().as_str().to_string();
                    let per = get_log_period(&log);
                    assert!(
                        17.0 < per && per < 19.0,
                        "log: {}\nperiod: {}",
                        log,
                        per,
                    );
                }

                it "can be delayed" {
                    let log = log2.borrow().as_str().to_string();
                    assert!(get_log_first(&log) > 10);
                    let per = get_log_period(&log);
                    assert!(
                        17.0 < per && per < 19.0,
                        "log: {}\nperiod: {}",
                        log,
                        per,
                    );
                }

                it "runs at most once per min period" {
                    let log = log3.borrow().as_str().to_string();
                    let per = get_log_period(&log);
                    assert!(
                        10.0 < per && per < 11.0,
                        "log: {}\nperiod: {}",
                        log,
                        per,
                    );
                }
            }

            context "note()" {
                before {
                    use_test_crate = true;
                    tested = "note";
                }

                it "outputs a new line" {
                    arg = Some("a new line");
                    output = TestCrateOutput::equals("\nfirst line\na new line");
                }

                it "quiet iff arg verbosity higher than set" {
                    arg = Some("quiet");
                    output = TestCrateOutput::equals("\nfirst line\nquiet");
                }
            }

            context "puts_clever()" {
                before {
                    use_test_crate = true;
                    tested = "puts_clever";
                }

                it "makes a new line iff dirty" {
                    arg = Some("iff dirty");
                    output = TestCrateOutput::equals("\nfirst line\niff dirty");
                }
            }

            it "set_verbosity() implicit" {}

            context "tell()" {
                it "output, see examples/status" {
                    tested = "tell";
                    use_test_crate = true;
                    output = TestCrateOutput::matches(
                        r#"\rfirst line\s*\rtale      "#
                    );
                }

                it "runs scheduled tasks" {
                }
            }

            context "unwrap_or_usage()" {
                before {
                    tested = "unwrap_or_usage";
                }

                context "with no error" {
                    it "just unwraps" {
                        let result: Result<i32, std::io::Error> = Ok(42);
                        assert_eq!(st.unwrap_or_usage(result), 42);
                    }
                }

                context "with error" {
                    before {
                        use_test_crate = true;
                    }

                    it "prints error before usage and error exit" {
                        error = Some("test-error");
                        output = TestCrateOutput::equals(
                            "test-error\nUsage: test [OPTIONS] [tested]\n"
                        );
                    }
                }
            }

            context "usage()" {
                before {
                    use_test_crate = true;
                    tested = "usage";
                }

                context "with no error" {
                    it "just prints usage and exit 0" {
                        output = TestCrateOutput::equals(
                            "Usage: test [OPTIONS] [tested]\n"
                        );
                    }
                }

                context "with error" {
                    it "prints error before usage and error exit" {
                        error = Some("test-error");
                        output = TestCrateOutput::equals(
                            "test-error\nUsage: test [OPTIONS] [tested]\n"
                        );
                    }
                }
            }

            after {
                if use_test_crate {
                    let used_test_crate = "examples/status";
                    let mut args: Vec<&str> = vec!["run", "--", tested];
                    if let Some(arg) = arg {
                        args.extend(["--arg", arg]);
                    }
                    let expect_error = error.is_some();
                    if expect_error {
                        args.extend(["--error", error.unwrap()]);
                    }
                    test_crate(
                        used_test_crate,
                        &args,
                        expect_error,
                        false,
                        output,
                    );
                }
            }
        }
    }
}
