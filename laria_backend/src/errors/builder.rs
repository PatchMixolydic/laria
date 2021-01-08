use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

use super::{span::Span, DiagnosticsContext, Level};

impl From<Level> for AnnotationType {
    fn from(level: Level) -> Self {
        match level {
            Level::ICE | Level::Error => AnnotationType::Error,
            Level::Warning => AnnotationType::Warning,
            Level::Help => AnnotationType::Help,
            Level::Info => AnnotationType::Info,
            Level::Note => AnnotationType::Note,
        }
    }
}

struct Label {
    contents: Option<String>,
    level: Level,
    span: Span,
}

impl<'label> From<&'label Label> for Annotation<'label> {
    fn from(label: &'label Label) -> Self {
        Annotation {
            label: Some(label.contents.as_ref().map_or("", |x| x.as_str())),
            id: None,
            annotation_type: label.level.into(),
        }
    }
}

impl<'label> From<&'label Label> for SourceAnnotation<'label> {
    fn from(label: &'label Label) -> Self {
        SourceAnnotation {
            label: label.contents.as_ref().map_or("", |x| x.as_str()),
            range: (label.span.start, label.span.start + label.span.length),
            annotation_type: label.level.into(),
        }
    }
}

#[must_use = "must emit the diagnostic for it to be seen"]
pub struct DiagnosticBuilder<'ctx, 'src> {
    title: String,
    level: Level,
    labels: Vec<Label>,
    footers: Vec<Label>,
    context: &'ctx DiagnosticsContext<'src>,
}

#[allow(dead_code)]
impl<'ctx, 'src> DiagnosticBuilder<'ctx, 'src> {
    pub(super) fn new(
        title: String,
        level: Level,
        context: &'ctx DiagnosticsContext<'src>,
    ) -> Self {
        Self {
            title,
            level,
            labels: Vec::new(),
            footers: Vec::new(),
            context,
        }
    }

    /// Add a label to a highlighted span with the current error level.
    pub fn span_label(mut self, span: Span, message: impl ToString) -> Self {
        self.labels.push(Label {
            contents: Some(message.to_string()),
            level: self.level,
            span,
        });
        self
    }

    /// Add a label to a highlighted span with the `Level::Note` level.
    pub fn note_label(mut self, span: Span, message: impl ToString) -> Self {
        self.labels.push(Label {
            contents: Some(message.to_string()),
            level: Level::Note,
            span,
        });
        self
    }

    /// Highlight the given span with the current error level and no label.
    pub fn with_span(mut self, span: Span) -> Self {
        self.labels.push(Label {
            contents: None,
            level: self.level,
            span,
        });
        self
    }

    /// Highlight the last character.
    pub fn with_eof_span(mut self) -> Self {
        self.labels.push(Label {
            contents: None,
            level: self.level,
            span: Span::new(self.context.source.len() - 1, 1),
        });
        self
    }

    /// Adds a `help: ...` footer.
    pub fn help(mut self, message: impl ToString) -> Self {
        self.footers.push(Label {
            contents: Some(message.to_string()),
            level: Level::Help,
            span: Span::empty(),
        });
        self
    }

    /// Adds a `note: ...` footer.
    pub fn note(mut self, message: impl ToString) -> Self {
        self.footers.push(Label {
            contents: Some(message.to_string()),
            level: Level::Note,
            span: Span::empty(),
        });
        self
    }

    pub fn emit(self) {
        let snippet = Snippet {
            title: Some(Annotation {
                label: Some(&self.title),
                id: None,
                annotation_type: self.level.into(),
            }),

            footer: self.footers.iter().map(Annotation::from).collect(),

            slices: vec![Slice {
                source: self.context.source,
                line_start: 1,
                origin: self.context.origin.as_deref(),
                fold: true,
                annotations: self.labels.iter().map(SourceAnnotation::from).collect(),
            }],

            opt: FormatOptions {
                color: true,
                ..Default::default()
            },
        };

        let dl = DisplayList::from(snippet);
        // TODO: customizable output
        eprintln!("{}", dl);
    }
}
