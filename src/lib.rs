//! A rust library for colorizing [`tracing_error::SpanTrace`] objects in the style
//! of [`color-backtrace`].
//!
//! ## Setup
//!
//! Add the following to your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! color-spantrace = "0.1"
//! tracing = "0.1.13"
//! tracing-error = "0.1.2"
//! tracing-subscriber = "0.2.5"
//! ```
//!
//! Setup a tracing subscriber with an `ErrorLayer`:
//!
//! ```rust
//! use tracing_error::ErrorLayer;
//! use tracing_subscriber::{prelude::*, registry::Registry};
//!
//! Registry::default().with(ErrorLayer::default()).init();
//! ```
//!
//! Create spans and enter them:
//!
//! ```rust
//! use tracing::instrument;
//! use tracing_error::SpanTrace;
//!
//! #[instrument]
//! fn foo() -> SpanTrace {
//!     SpanTrace::capture()
//! }
//! ```
//!
//! And finally colorize the `SpanTrace`:
//!
//! ```rust
//! use tracing_error::SpanTrace;
//!
//! let span_trace = SpanTrace::capture();
//! println!("{}", color_spantrace::colorize(&span_trace));
//! ```
//!
//! [`tracing_error::SpanTrace`]: https://docs.rs/tracing-error/*/tracing_error/struct.SpanTrace.html
//! [`color-backtrace`]: https://github.com/athre0z/color-backtrace
#![doc(html_root_url = "https://docs.rs/color-spantrace/0.1.4")]
#![warn(
    missing_debug_implementations,
    missing_docs,
    missing_doc_code_examples,
    rust_2018_idioms,
    unreachable_pub,
    bad_style,
    const_err,
    dead_code,
    improper_ctypes,
    non_shorthand_field_patterns,
    no_mangle_generic_items,
    overflowing_literals,
    path_statements,
    patterns_in_fns_without_body,
    private_in_public,
    unconditional_recursion,
    unused,
    unused_allocation,
    unused_comparisons,
    unused_parens,
    while_true
)]
use std::env;
use std::fmt;
use std::fs::File;
use std::io::{BufRead, BufReader, ErrorKind};
use tracing_error::SpanTrace;
use once_cell::sync::OnceCell;
use owo_colors::{style, Style, XtermColors};

static STYLES: OnceCell<Styles> = OnceCell::new();

// XXX now that we support text effects, "color scheme" doesn't seem accurate anymore. Let me know if you'd prefer a different name. The good thing about `Styles` is, that it's pretty short.

/// A struct that represents styles that should be used by `color_spantrace`
#[derive(Debug, Copy, Clone, Default)]
pub struct Styles {
    file: Style,
    line_number: Style,
    target: Style,
    fields: Style,
    active_line: Style,
}

impl Styles {
    /// Create blank styles
    pub fn new() -> Self {
        Self::default()
    }

    /// Styles for a dark background. This is the default
    pub fn dark() -> Self {
        Self {
            file:         style().purple(),
            line_number:  style().purple(),
            active_line:  style().white().bold(),

            // XXX your trait from this file (which I have removed now) produced "xterm" colors. `owo_colors` documents the `xterm` module as follows: "XTerm 256-bit colors. Not as widely supported as standard ANSI but contains 240 more colors." (see *). Maybe it would be best to switch to something else now. In this case, the colors also have to be changed in `owo_colors`
            // * https://github.com/jam1garner/owo-colors/blob/c0db9127cbc355dec36fbfa45b9adc534b72c4ad/src/colors.rs#L173
            target:       style().color(XtermColors::UserBrightRed),
            fields:       style().color(XtermColors::UserBrightCyan),
        }
    }

    // XXX same as with `light` in `color_eyre`
    /// Styles for a light background
    pub fn light() -> Self {
        Self {
            file:         style().purple(),
            line_number:  style().purple(),
            target:       style().red(),
            fields:       style().blue(),
            active_line:  style().bold(),
        }
    }

    /// Styles printed paths
    pub fn file(mut self, style: Style) -> Self {
        self.file = style;
        self
    }

    /// Styles the line number of a file
    pub fn line_number(mut self, style: Style) -> Self {
        self.line_number = style;
        self
    }

    /// Styles the target (i.e. the module and function name, and so on)
    pub fn target(mut self, style: Style) -> Self {
        self.target = style;
        self
    }

    // XXX is this correct?
    /// Styles fields of the `tracing` crate (in the context of `color_spantrace`, the arguments of functions and methods)
    pub fn fields(mut self, style: Style) -> Self {
        self.fields = style;
        self
    }

    /// Styles the selected line of displayed code
    pub fn active_line(mut self, style: Style) -> Self {
        self.active_line = style;
        self
    }
}

/// Sets the global styles. This can only be set once and otherwise fails (`Err` contains the `Styles` that was passed to `set_styles`). Note: `colorize` sets the global styles implicitly, if they were not set already. So calling `colorize` and then `set_styles` fails
pub fn set_styles(styles: Styles) -> Result<(), Styles> {
    STYLES.set(styles)
}

/// Display a [`SpanTrace`] with colors and source
///
/// This function returns an `impl Display` type which can be then used in place of the original
/// SpanTrace when writing it too the screen or buffer.
///
/// # Example
///
/// ```rust
/// use tracing_error::SpanTrace;
///
/// let span_trace = SpanTrace::capture();
/// println!("{}", color_spantrace::colorize(&span_trace));
/// ```
///
/// Note: `colorize` sets the global styles implicitly, if they were not set already.
///
/// [`SpanTrace`]: https://docs.rs/tracing-error/*/tracing_error/struct.SpanTrace.html
pub fn colorize(span_trace: &SpanTrace) -> impl fmt::Display + '_ {
    let styles = *STYLES.get_or_init(Styles::dark);
    ColorSpanTrace { span_trace, styles }
}

struct ColorSpanTrace<'a> {
    span_trace: &'a SpanTrace,
    styles: Styles,
}

macro_rules! try_bool {
    ($e:expr, $dest:ident) => {{
        let ret = $e.unwrap_or_else(|e| $dest = Err(e));

        if $dest.is_err() {
            return false;
        }

        ret
    }};
}

struct Frame<'a> {
    metadata: &'a tracing_core::Metadata<'static>,
    fields: &'a str,
    styles: Styles,
}

/// Defines how verbose the backtrace is supposed to be.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Verbosity {
    /// Print a small message including the panic payload and the panic location.
    Minimal,
    /// Everything in `Minimal` and additionally print a backtrace.
    Medium,
    /// Everything in `Medium` plus source snippets for all backtrace locations.
    Full,
}

impl Verbosity {
    fn lib_from_env() -> Self {
        Self::convert_env(
            env::var("RUST_LIB_BACKTRACE")
                .or_else(|_| env::var("RUST_BACKTRACE"))
                .ok(),
        )
    }

    fn convert_env(env: Option<String>) -> Self {
        match env {
            Some(ref x) if x == "full" => Verbosity::Full,
            Some(_) => Verbosity::Medium,
            None => Verbosity::Minimal,
        }
    }
}

impl Frame<'_> {
    fn print(&self, i: u32, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print_header(i, f)?;
        self.print_fields(f)?;
        self.print_source_location(f)?;
        Ok(())
    }

    fn print_header(&self, i: u32, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:>2}: {}{}{}",
            i,
            self.styles.target.style(self.metadata.target()),
            self.styles.target.style("::"),
            self.styles.target.style(self.metadata.name()),
        )
    }

    fn print_fields(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.fields.is_empty() {
            write!(f, " with {}", self.styles.fields.style(self.fields))?;
            // XXX via `OwoColorize` there is also the following:
            // write!(f, " with {}", self.fields.style(self.styles.fields));
            // I think the version above is a bit easier to read, but let me
            // know if you prefer the alternative.
        }

        Ok(())
    }

    fn print_source_location(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(file) = self.metadata.file() {
            let lineno = self
                .metadata
                .line()
                .map_or("<unknown line>".to_owned(), |x| x.to_string());
            write!(
                f,
                "\n    at {}:{}",
                self.styles.file.style(file),
                self.styles.line_number.style(lineno),
            )?;
        } else {
            write!(f, "\n    at <unknown source file>")?;
        }

        Ok(())
    }

    fn print_source_if_avail(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (lineno, filename) = match (self.metadata.line(), self.metadata.file()) {
            (Some(a), Some(b)) => (a, b),
            // Without a line number and file name, we can't sensibly proceed.
            _ => return Ok(()),
        };

        let file = match File::open(filename) {
            Ok(file) => file,
            Err(ref e) if e.kind() == ErrorKind::NotFound => return Ok(()),
            e @ Err(_) => e.unwrap(),
        };

        use std::fmt::Write;

        // Extract relevant lines.
        let reader = BufReader::new(file);
        let start_line = lineno - 2.min(lineno - 1);
        let surrounding_src = reader.lines().skip(start_line as usize - 1).take(5);
        let mut buf = String::new();
        for (line, cur_line_no) in surrounding_src.zip(start_line..) {
            if cur_line_no == lineno {
                write!(
                    &mut buf,
                    "{:>8} > {}",
                    cur_line_no.to_string(),
                    line.unwrap()
                )?;
                write!(f, "\n{}", self.styles.active_line.style(&buf))?;
                buf.clear();
            } else {
                write!(f, "\n{:>8} │ {}", cur_line_no, line.unwrap())?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for ColorSpanTrace<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut err = Ok(());
        let mut span = 0;

        writeln!(f, "{:━^80}\n", " SPANTRACE ")?;
        self.span_trace.with_spans(|metadata, fields| {
            let frame = Frame { metadata, fields, styles: self.styles };

            if span > 0 {
                try_bool!(write!(f, "\n",), err);
            }

            try_bool!(frame.print(span, f), err);

            if Verbosity::lib_from_env() == Verbosity::Full {
                try_bool!(frame.print_source_if_avail(f), err);
            }

            span += 1;
            true
        });

        err
    }
}