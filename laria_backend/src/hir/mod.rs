//! HIR is the High-level Intermediate Representation, inspired by
//! [Rust's HIR][rust-hir]. While very similar to [the AST][ast], it is
//! different in a couple of key aspects:
//!
//! * Some constructs, such as partial applications, are desugared to make them
//!   easier for the compiler to work with.
//! * HIR has type information and is used for typechecking scripts.
//!
//! Please note that the HIR is currently a work in progress. There are
//! a few hacks here and there that will be cleaned up soon, namely
//! the use of raw identifiers rather than unique paths, which can
//! confuse analysis passes when dealing with complex (or even simple)
//! scripts.
//!
//! [rust-hir]: https://rustc-dev-guide.rust-lang.org/hir.html

use crate::parser::ast;

mod hir_tree;
mod lower;
mod typecheck;
mod types;

pub(crate) fn validate(ast: ast::Script, source: &str) {
    let (mut ir, ty_env) = lower::lower_ast(ast, source);
    typecheck::typecheck(ir, ty_env, source);
}
