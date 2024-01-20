use regex::Regex;
use std::fmt::Display;
use std::path::Path;

/// What output to expect from the command's `stdout`
pub enum TestCommandOutput {
    /// Output is expected to equal the `String`.
    EqualTo(String),
    /// Output is expected not to equal the `String`.
    NotEqualTo(String),
    /// Output is expected to match a `Regex` compiled from the `String`.
    Matches(String),
    /// Output is expected not to match a `Regex` compiled from the `String`.
    NotMatches(String),
    /// Output is expected to start with the `String`.
    StartsWith(String),
    /// Output is expected not to start with the `String`.
    NotStartsWith(String),
    /// Output is ignored.
    Whatever,
}

impl TestCommandOutput {
    /// `EqualTo(s.to_string())`
    pub fn equal_to<T: AsRef<str> + Display>(s: T) -> Self {
        Self::EqualTo(s.to_string())
    }
    /// `NotEqualTo(s.to_string())`
    pub fn not_equal_to<T: AsRef<str> + Display>(s: T) -> Self {
        Self::NotEqualTo(s.to_string())
    }
    /// `Matches(s.to_string())`
    pub fn matches<T: AsRef<str> + Display>(s: T) -> Self {
        Self::Matches(s.to_string())
    }
    /// `NotMatches(s.to_string())`
    pub fn not_matches<T: AsRef<str> + Display>(s: T) -> Self {
        Self::NotMatches(s.to_string())
    }
    /// `StartsWith(s.to_string())`
    pub fn starts_with<T: AsRef<str> + Display>(s: T) -> Self {
        Self::StartsWith(s.to_string())
    }
    /// `NotStartsWith(s.to_string())`
    pub fn not_starts_with<T: AsRef<str> + Display>(s: T) -> Self {
        Self::NotStartsWith(s.to_string())
    }
}

/// `std::fs::create_dir_all()` unwrapped or panicking with failing path.
pub fn create_dir_all(dir: &Path) {
    std::fs::create_dir_all(dir).expect(&format!(
        "'create_dir_all(\"{}\")' failed",
        std::fs::canonicalize(dir)
            .unwrap_or_else(|_| dir.to_path_buf())
            .display(),
    ));
}

/// Execute the command, expecting
pub fn test_command(
    directory: &str,
    name: &str,
    args: &[&str],
    expect_error: bool,
    show_stdout: bool,
    expect_output: TestCommandOutput,
    fail_msg: Option<&str>,
) {
    let mut default_msg = String::new();
    let fail_msg = fail_msg.unwrap_or_else(|| {
        let mut argstr = String::new();
        if !args.is_empty() {
            argstr = argstr
                + " "
                + &args.iter().map(|s| *s).collect::<Vec<_>>().join(" ");
        }
        default_msg = format!("cd {}; {} {} failed", directory, name, argstr);
        &default_msg
    });
    let mut command = std::process::Command::new(name);
    command.current_dir(directory);
    for arg in args {
        command.arg(arg);
    }
    let output = command.output().expect(fail_msg);
    let exit_code = output.status.code();
    if exit_code.is_none() || (expect_error ^ (exit_code.unwrap() != 0)) {
        if show_stdout {
            eprintln!("stdout: {}", String::from_utf8_lossy(&output.stdout));
        }
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        panic!("exit code: {}", exit_code.unwrap_or(-11111));
    }
    let output = String::from_utf8_lossy(&if expect_error {
        &output.stderr
    } else {
        &output.stdout
    });
    match expect_output {
        TestCommandOutput::EqualTo(expected) => {
            assert_eq!(output, expected);
        }
        TestCommandOutput::NotEqualTo(expected) => {
            assert_ne!(output, expected);
        }
        TestCommandOutput::Matches(expected) => {
            let regex = Regex::new(&expected);
            assert!(regex.is_ok());
            assert!(regex.unwrap().is_match(&output));
        }
        TestCommandOutput::NotMatches(expected) => {
            let regex = Regex::new(&expected);
            assert!(regex.is_ok());
            assert!(!regex.unwrap().is_match(&output));
        }
        TestCommandOutput::StartsWith(expected) => {
            assert!(output.starts_with(&expected));
        }
        TestCommandOutput::NotStartsWith(expected) => {
            assert!(!output.starts_with(&expected));
        }
        _ => (),
    }
}

pub fn test_crate(
    crate_dir: &str,
    cargo_args: &[&str],
    expect_error: bool,
    show_stdout: bool,
    expect_output: TestCommandOutput,
) {
    use std::process::Command;

    Command::new("cargo")
        .current_dir(crate_dir)
        .arg("update")
        .output()
        .ok();
    let mut args = vec!["--quiet"];
    args.extend(cargo_args);
    test_command(
        crate_dir,
        "cargo",
        args.as_slice(),
        expect_error,
        show_stdout,
        expect_output,
        None,
    );
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
    form: Vec<FormPart>,
}

impl TestRequest {
    pub fn new(url: &str) -> Self {
        Self {
            cookies: false,
            data: Vec::new(),
            form: Vec::new(),
            get: true,
            headers: Vec::new(),
            redir: true,
            url: url.to_string(),
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

    pub fn fetch_response(self) -> TestResponse {
        use lazy_static::lazy_static;
        lazy_static! {
            static ref OUTPUT: Regex = Regex::new(
r#"((?s).*)"\n__variables__\nstatus: (.*)\ncontent-type: (.*)\nredirect: (.*)""#,
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
                \ncontent-type: %{content_type}\
                \nredirect: %{redirect_url}\
            \"",
        );
        if self.cookies {
            create_dir_all(Path::new("tmp"));
            curl.arg("--cookie-jar")
                .arg("tmp/__test-cookies.txt")
                .arg("--cookie")
                .arg("tmp/__test-cookies.txt");
        }
        if self.get {
            curl.arg("--get");
        } else if self.data.is_empty() && self.form.is_empty() {
            curl.arg("--request").arg("POST");
        }
        if self.redir {
            curl.arg("--location");
        }
        for item in self.data {
            curl.arg("--data-urlencode").arg(&format!("{}", item));
        }
        for part in self.form {
            curl.arg("--form").arg(&format!(
                "{}={}{}",
                &part.name,
                if part.file { "@" } else { "" },
                &part.value,
            ));
        }
        curl.arg(&self.url);
//eprintln!("curl {}", curl.get_args().map(|a| a.to_string_lossy()).collect::<Vec<_>>().join(" "));
        let output = curl.output().unwrap().stdout;
        let output = String::from_utf8_lossy(&output);
        let parts = OUTPUT.captures(&output).unwrap();
        TestResponse {
            body: parts.get(1).unwrap().as_str().to_string(),
            status: parts.get(2).unwrap().as_str().to_string(),
            content_type: parts.get(3).unwrap().as_str().to_string(),
            redirect: parts.get(4).unwrap().as_str().to_string(),
        }
    }

    pub fn form_part(mut self, name: &str, value: &str, file: bool) -> Self {
        self.get = false;
        self.form.push(FormPart {
            name: name.to_string(),
            value: value.to_string(),
            file,
        });
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

    pub fn post(mut self) -> Self {
        self.get = false;
        self
    }
}

#[derive(Clone, Debug)]
pub struct TestResponse {
    body: String,
    content_type: String,
    redirect: String,
    status: String,
}

impl TestResponse {
    pub fn body(&self) -> &str {
        self.body.as_str()
    }

    pub fn content_type(&self) -> &str {
        self.content_type.as_str()
    }

    pub fn redirect(&self) -> &str {
        self.redirect.as_str()
    }

    pub fn status(&self) -> &str {
        self.status.as_str()
    }
}

#[derive(Clone, Debug)]
pub struct FormPart {
    name: String,
    value: String,
    file: bool,
}
