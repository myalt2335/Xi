use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use serde::Deserialize;

use crate::language::Span;

#[derive(Debug, Deserialize)]
pub struct CompilerDiagnostic {
    pub code: String,
    pub message: String,
    pub span: Span,
}

pub fn check(
    compiler: &Path,
    filename: &Path,
    source: &str,
) -> Result<Vec<CompilerDiagnostic>, String> {
    let mut command = Command::new(compiler);
    command
        .arg(filename)
        .arg("--stdin")
        .arg("--check")
        .arg("--diagnostics-json")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        command.creation_flags(0x0800_0000);
    }

    let mut child = command
        .spawn()
        .map_err(|error| format!("could not start {}: {error}", compiler.display()))?;
    child
        .stdin
        .take()
        .ok_or_else(|| "compiler stdin was unavailable".to_string())?
        .write_all(source.as_bytes())
        .map_err(|error| format!("could not send source to compiler: {error}"))?;

    let output = child
        .wait_with_output()
        .map_err(|error| format!("could not wait for compiler: {error}"))?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    serde_json::from_str(stdout.trim()).map_err(|error| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        format!(
            "compiler returned invalid diagnostics JSON: {error}; stderr: {}",
            stderr.trim()
        )
    })
}
