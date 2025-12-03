use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self, emit_to_write_style};

pub trait IntoDiagnostic {
    fn to_diagnostic(&self, file_id: usize) -> codespan_reporting::diagnostic::Diagnostic<usize>;
}

pub fn show_diagnostics<T: IntoDiagnostic>(
    source: impl AsRef<str>,
    filename: impl AsRef<str>,
    errors: &[T],
) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(filename.as_ref(), source);

    let mut writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();

    for error in errors {
        let diagnostic = error.to_diagnostic(file_id);
        emit_to_write_style(&mut writer, &config, &files, &diagnostic).unwrap();
    }
}
