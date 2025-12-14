use std::process::Stdio;

use anyhow::bail;
use libtest_mimic::Failed;

mod test_harness_core;

fn main() {
    test_harness_core::run_harness_on_dir("tests/llvm");
}
