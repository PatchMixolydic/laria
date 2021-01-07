mod builder;
mod span;

pub use self::builder::DiagnosticBuilder;
pub use self::span::Span;

#[derive(Clone, Copy)]
enum Level {
    ICE,
    Error,
    Warning,
    Help,
    Info,
    Note
}

pub struct DiagnosticsContext<'src> {
    source: &'src str,
    origin: Option<String>
}

#[allow(dead_code)]
impl<'src> DiagnosticsContext<'src> {
    pub fn new(source: &'src str, origin: Option<String>) -> Self {
        Self { source, origin }
    }

    pub fn build_ice(&self, message: impl ToString) -> DiagnosticBuilder {
        DiagnosticBuilder::new(message.to_string(), Level::ICE, self)
            .note("this is an internal error")
            .note("a bug report would be highly appreciated:\nhttps://github.com/PatchMixolydic/triplicata/issues/new")
    }

    pub fn build_ice_span(&self, span: Span, message: impl ToString) -> DiagnosticBuilder {
        self.build_ice(message).with_span(span)
    }

    // The below is quite repetitive, but using a macro causes rust-analyzer
    // to be unable to find these functions :(

    pub fn build_error(&self, message: impl ToString) -> DiagnosticBuilder {
        DiagnosticBuilder::new(message.to_string(), Level::Error, self)
    }

    pub fn build_error_span(&self, span: Span, message: impl ToString) -> DiagnosticBuilder {
        self.build_error(message).with_span(span)
    }

    pub fn build_warning(&self, message: impl ToString) -> DiagnosticBuilder {
        DiagnosticBuilder::new(message.to_string(), Level::Warning, self)
    }

    pub fn build_warning_span(&self, span: Span, message: impl ToString) -> DiagnosticBuilder {
        self.build_warning(message).with_span(span)
    }

    pub fn build_help(&self, message: impl ToString) -> DiagnosticBuilder {
        DiagnosticBuilder::new(message.to_string(), Level::Help, self)
    }

    pub fn build_help_span(&self, span: Span, message: impl ToString) -> DiagnosticBuilder {
        self.build_help(message).with_span(span)
    }

    pub fn build_info(&self, message: impl ToString) -> DiagnosticBuilder {
        DiagnosticBuilder::new(message.to_string(), Level::Info, self)
    }

    pub fn build_info_span(&self, span: Span, message: impl ToString) -> DiagnosticBuilder {
        self.build_info(message).with_span(span)
    }

    pub fn build_note(&self, message: impl ToString) -> DiagnosticBuilder {
        DiagnosticBuilder::new(message.to_string(), Level::Note, self)
    }

    pub fn build_note_span(&self, span: Span, message: impl ToString) -> DiagnosticBuilder {
        self.build_note(message).with_span(span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let ctx = DiagnosticsContext::new("(lambda (x) 1 + x)", None);
        ctx.build_error("use of infix operator detected")
            .span_label(Span::new(12, 5), "no")
            .help("don't do that")
            .note("wouldn't have happened in Rust")
            .emit();
    }
}
