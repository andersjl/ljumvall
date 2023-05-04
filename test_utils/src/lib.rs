use regex::Regex;
use std::fmt::Display;

/// What output to expect from the tested binary's `stdout`
pub enum TestCrateOutput {
    /// The ouput is expected to equal the `String`.
    Equals(String),
    /// The ouput is expected to match a `Regex` compiled from the `String`.
    Matches(String),
    /// The ouput is expected to start with the `String`.
    StartsWith(String),
    /// The ouput is ignored.
    Whatever,
}

impl TestCrateOutput {
    /// `Equals(s.to_string())`
    pub fn equals<T: AsRef<str> + Display>(s: T) -> Self {
        Self::Equals(s.to_string())
    }
    /// `Matches(s.to_string())`
    pub fn matches<T: AsRef<str> + Display>(s: T) -> Self {
        Self::Matches(s.to_string())
    }
    /// `StartsWith(s.to_string())`
    pub fn starts_with<T: AsRef<str> + Display>(s: T) -> Self {
        Self::StartsWith(s.to_string())
    }
}

pub fn create_dir_all(dir: &str) {
    std::fs::create_dir_all(dir).expect(&format!(
        "'create_dir_all(\"{}\")' failed",
        std::fs::canonicalize(dir)
            .unwrap_or_else(|_| std::path::Path::new(dir).to_path_buf())
            .display(),
    ));
}

pub fn test_crate(
    crate_dir: &str,
    cargo_args: &[&str],
    expect_error: bool,
    show_stdout: bool,
    expect_output: TestCrateOutput,
) {
    use std::process::Command;

    Command::new("cargo")
        .current_dir(crate_dir)
        .arg("update")
        .output()
        .ok();
    let output = Command::new("cargo")
        .current_dir(crate_dir)
        .arg("--quiet")
        .args(cargo_args)
        .output()
        .expect(&format!(
            "*** 'cd {}; cargo {}' failed",
            crate_dir,
            cargo_args.join(" "),
        ));
    let exit_code = output.status.code();
    if exit_code.is_none() || (expect_error ^ (exit_code.unwrap() != 0)) {
        if show_stdout {
            eprintln!(
                "tested program stdout: {}",
                String::from_utf8_lossy(&output.stdout),
            );
        }
        eprintln!(
            "tested program stderr: {}",
            String::from_utf8_lossy(&output.stderr),
        );
        panic!(
            "tested program exit code: {}",
            exit_code.unwrap_or(-1111111111)
        );
    }
    let output = String::from_utf8_lossy(&if expect_error {
        &output.stderr
    } else {
        &output.stdout
    });
    match expect_output {
        TestCrateOutput::Equals(expected) => {
            assert_eq!(output, expected);
        }
        TestCrateOutput::Matches(expected) => {
            let regex = Regex::new(&expected);
            assert!(regex.is_ok());
            assert!(regex.unwrap().is_match(&output));
        }
        TestCrateOutput::StartsWith(expected) => {
            assert!(output.starts_with(&expected));
        }
        _ => (),
    }
}

#[macro_export]
macro_rules! test_http_server {
    ($server_path:expr, $show:expr, $( $request:expr, $test:expr),* $(,)?) => {
        {
            let mut reqs = Vec::new();
        $(  reqs.push($request); )*
            let mut resps =
                ljumvall_test_utils::run_http_server($server_path, $show, reqs);
        $(  $test(&resps.drain(..1).next().unwrap()); )*
            println!("OK");
        }
    };
}

pub fn random_hex() -> String {
    use rand::{thread_rng, Rng};
    format!("{:0>16x}", thread_rng().gen::<u64>())
}

pub fn run_http_server(
    server_dir: &str,
    show_build: bool,
    requests: Vec<TestRequest>,
) -> Vec<TestResponse> {
    use std::process::Command;
    use std::thread::sleep;
    use std::time::Duration;

    let mut result: Vec<TestResponse> = Vec::new();
    println!("building {} ... ", server_dir);
    Command::new("cargo")
        .current_dir(server_dir)
        .arg("update")
        .output()
        .ok();
    let build_output = Command::new("cargo")
        .current_dir(server_dir)
        .arg("build")
        .arg("--release")
        .arg("--quiet")
        .output()
        .expect("build failed");
    match build_output.status.code() {
        Some(code) if code == 0 => (),
        _ => {
            if show_build {
                eprintln!(
                    "build output: {}",
                    String::from_utf8_lossy(&build_output.stdout),
                );
            }
        }
    }
    let _ = Command::new("rm").arg("tmp/__test-cookies.txt").output();
    println!("    OK");
    println!("running {} ... ", server_dir);
    if let Ok(mut run) = Command::new("cargo")
        .current_dir(server_dir)
        .arg("run")
        .arg("--release")
        .arg("--quiet")
        .spawn()
    {
        sleep(Duration::from_secs(2));
        for req in requests {
            result.push(req.fetch_response());
        }
        run.kill().unwrap();
    } else {
        println!("*** cargo run did not start");
    }
    result
}

#[derive(Clone, Debug)]
pub struct TestRequest {
    url: String,
    data: Vec<String>,
    cookies: bool,
    get: bool,
    redir: bool,
    headers: Vec<String>,
}

impl TestRequest {
    pub fn new(url: &str) -> Self {
        Self {
            url: url.to_string(),
            data: Vec::new(),
            cookies: false,
            get: true,
            redir: true,
            headers: Vec::new(),
        }
    }

    pub fn cookies(mut self) -> Self {
        self.cookies = true;
        self
    }

    pub fn data(mut self, name: &str, value: &str) -> Self {
        self.data.push(format!("{}={}", name, value));
        self
    }

    pub fn header(mut self, name: &str, value: &str) -> Self {
        self.headers.push(format!("{}: {}", name, value));
        self
    }

    pub fn no_redirect(mut self) -> Self {
        self.redir = false;
        self
    }

    pub fn fetch_response(self) -> TestResponse {
        use lazy_static::lazy_static;
        lazy_static! {
            static ref OUTPUT: Regex = Regex::new(
                r#"((?s).*)"\n__variables__\nstatus: (.*)\nredirect: (.*)""#,
            )
            .unwrap();
        }

        use std::process::Command;
        let mut curl = Command::new("curl");
        curl.arg("--silent");
        for header in self.headers {
            curl.arg("--header").arg(&format!("\"{}\"", header));
        }
        curl.arg("--write-out").arg(
            "\"\
                \n__variables__\
                \nstatus: %{response_code}\
                \nredirect: %{redirect_url}\
            \"",
        );
        if self.cookies {
            create_dir_all("tmp");
            curl.arg("--cookie-jar")
                .arg("tmp/__test-cookies.txt")
                .arg("--cookie")
                .arg("tmp/__test-cookies.txt");
        }
        if self.get {
            curl.arg("--get");
        } else if self.data.is_empty() {
            curl.arg("--request").arg("POST");
        }
        if self.redir {
            curl.arg("--location");
        }
        for item in self.data {
            curl.arg("--data-urlencode").arg(&format!("{}", item));
        }
        curl.arg(&self.url);
        //eprintln!("curl {}", curl.get_args().map(|a| a.to_string_lossy()).collect::<Vec<_>>().join(" "));
        let output = curl.output().unwrap().stdout;
        let output = String::from_utf8_lossy(&output);
        let parts = OUTPUT.captures(&output).unwrap();
        TestResponse {
            body: parts.get(1).unwrap().as_str().to_string(),
            status: parts.get(2).unwrap().as_str().to_string(),
            redirect: parts.get(3).unwrap().as_str().to_string(),
        }
    }

    pub fn post(mut self) -> Self {
        self.get = false;
        self
    }
}

#[derive(Clone, Debug)]
pub struct TestResponse {
    body: String,
    redirect: String,
    status: String,
}

impl TestResponse {
    pub fn body(&self) -> &str {
        self.body.as_str()
    }

    pub fn redirect(&self) -> &str {
        self.redirect.as_str()
    }

    pub fn status(&self) -> &str {
        self.status.as_str()
    }
}
