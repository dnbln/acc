use std::process::Stdio;

use anyhow::bail;
use libtest_mimic::Failed;

fn run_test_case(
    test_name: String,
    input_path: impl AsRef<std::path::Path>,
    output_path: impl AsRef<std::path::Path>,
    target_bin: impl AsRef<std::path::Path>,
    bless_mode: bool,
) -> Result<(), anyhow::Error> {
    use std::fs;
    use std::process::Command;

    let input_path = input_path.as_ref();
    let output_path = output_path.as_ref();
    let target_bin = target_bin.as_ref();

    let test_file = std::fs::read_to_string(input_path).unwrap();
    let mut args = Vec::new();

    enum ExpectFailInfo {
        No,
        Yes,
        YesWithMessage(String),
    }

    impl ExpectFailInfo {
        fn is_yes(&self) -> bool {
            matches!(
                self,
                ExpectFailInfo::Yes | ExpectFailInfo::YesWithMessage(_)
            )
        }
    }

    let mut expect_fail = ExpectFailInfo::No;
    for line in test_file
        .lines()
        .take_while(|it| it.trim().starts_with("//@"))
        .filter_map(|it| it.trim().strip_prefix("//@"))
    {
        if let Some(rest) = line.trim().strip_prefix("FAIL: ") {
            expect_fail = ExpectFailInfo::YesWithMessage(rest.to_string());
        } else if line.trim().starts_with("FAIL") {
            expect_fail = ExpectFailInfo::Yes;
        }
        if let Some(rest) = line.trim().strip_prefix("ARGS: ") {
            args.extend(rest.split_whitespace());
        }
    }

    let output = Command::new(target_bin)
        .arg(input_path)
        .args(&args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .env("NO_COLOR", "1")
        .output()
        .expect("Failed to execute test case");

    let failed = !output.status.success();
    if failed {
        match expect_fail {
            ExpectFailInfo::No => {
                bail!(
                    "Test case '{}' failed to execute.\nStderr:\n{}",
                    test_name,
                    String::from_utf8_lossy(&output.stderr)
                );
            }
            ExpectFailInfo::Yes => {}
            ExpectFailInfo::YesWithMessage(ref msg) => {
                let stderr = String::from_utf8_lossy(&output.stderr);
                if !stderr.contains(msg) {
                    bail!(
                        "Test case '{}' failed, but error message did not match.\nExpected to find: '{}'\nActual stderr:\n{}",
                        test_name,
                        msg,
                        stderr
                    );
                }
            }
        }
    } else if !failed && expect_fail.is_yes() {
        bail!(
            "Test case '{}' was expected to fail but succeeded.\nStdout:\n{}",
            test_name,
            String::from_utf8_lossy(&output.stdout)
        );
    }

    let actual_stdout = String::from_utf8_lossy(&output.stdout);
    let actual_stderr = String::from_utf8_lossy(&output.stderr);

    let actual_output = format!("STDOUT:\n{}\nSTDERR:\n{}", actual_stdout, actual_stderr);

    if bless_mode {
        fs::write(output_path, &actual_output).unwrap_or_else(|_| {
            panic!(
                "Failed to write blessed output for test case '{}'",
                test_name
            )
        });
    } else {
        let expected_output = fs::read_to_string(output_path).unwrap_or_else(|_| {
            panic!(
                "Failed to read expected output for test case '{}'",
                test_name
            )
        });
        if actual_output != expected_output {
            bail!(
                "Test case '{}' failed.\nExpected Output:\n{}\nActual Output:\n{}",
                test_name,
                expected_output,
                actual_output
            );
        }
    }

    Ok(())
}

fn main() {
    let args = libtest_mimic::Arguments::from_args();
    let mut tests = vec![];

    let target_bin = env!("CARGO_BIN_EXE_acc");
    let bless_mode =
        std::env::var("BLESS").is_ok_and(|it| ["1", "true", "yes"].contains(&it.as_str()));

    for entry in std::fs::read_dir("tests").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("c") {
            let test_name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap()
                .to_string();
            let output_path = path.with_extension("out");
            tests.push(libtest_mimic::Trial::test(test_name.clone(), move || {
                run_test_case(test_name, path, output_path, target_bin, bless_mode)
                    .map_err(Failed::from)
            }));
        }
    }

    libtest_mimic::run(&args, tests).exit();
}
