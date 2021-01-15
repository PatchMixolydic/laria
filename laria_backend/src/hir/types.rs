use std::collections::HashMap;

use crate::errors::{DiagnosticsContext, Span};

pub(super) type TypeId = usize;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Type {
    Variable(u64),
    Integer,
    Boolean,
    Float,
    String,
    Function(TypeId, TypeId),
    Tuple(Vec<TypeId>),
}

impl Type {
    /// Convenience function to create the unit type.
    pub(super) const fn unit() -> Self {
        Self::Tuple(Vec::new())
    }

    /// Returns a string, suitable for display, that represents this type.
    /// This isn't a [`Display`] or [`ToString`] implementation since this
    /// requires access to a [`TypeEnvironment`].
    ///
    /// [`Display`]: std::fmt::Display
    fn to_string(&self, ty_env: &TypeEnvironment) -> String {
        match self {
            Type::Variable(id) => format!("{{type variable #{}}}", id),
            Type::Integer => "{integer}".to_owned(),
            Type::Boolean => "bool".to_owned(),
            Type::Float => "{float}".to_owned(),
            Type::String => "string".to_owned(),

            Type::Function(args_id, return_id) => {
                let args_ty = ty_env.get_type(*args_id).to_string(ty_env);
                let ret_ty = ty_env.get_type(*return_id).to_string(ty_env);
                format!("{} -> {}", args_ty, ret_ty)
            },

            Type::Tuple(contents) => {
                let contents_string = contents
                    .into_iter()
                    .map(|id| ty_env.get_type(*id).to_string(ty_env))
                    .intersperse(", ".to_owned())
                    .collect::<String>();

                format!("({})", contents_string)
            },
        }
    }
}

/// Holds information on the type universe.
pub(super) struct TypeEnvironment<'src> {
    type_id_to_type: Vec<Type>,
    // Let's assume name resolution was done already,
    // so we don't have any invalid identifiers
    // TODO: not likely; consider switching to something
    // like Rust's `DefId`, which is more or less an
    // interned path to the ident
    ident_to_type_id: HashMap<String, TypeId>,
    next_type_var_id: u64,
    error_ctx: DiagnosticsContext<'src>,
}

impl<'src> TypeEnvironment<'src> {
    pub(super) fn new(source: &'src str) -> Self {
        Self {
            type_id_to_type: Vec::new(),
            ident_to_type_id: HashMap::new(),
            next_type_var_id: 0,
            error_ctx: DiagnosticsContext::new(source, None),
        }
    }

    /// Gets the given type or adds to the universe if it doesn't exist.
    /// Returns the type's [`TypeId`].
    pub(super) fn get_or_add_type(&mut self, ty: Type) -> TypeId {
        // TODO: might be slow
        for i in 0..self.type_id_to_type.len() {
            if self.type_id_to_type[i] == ty {
                return i;
            }
        }

        self.type_id_to_type.push(ty);
        self.type_id_to_type.len() - 1
    }

    /// Adds a new type variable to the universe with a unique variable id.
    /// Returns the variable's [`TypeId`] (not its variable id).
    pub(super) fn add_new_type_variable(&mut self) -> TypeId {
        let ty = Type::Variable(self.next_type_var_id);
        self.next_type_var_id += 1;
        self.get_or_add_type(ty)
    }

    /// Given a type id, get the concrete type from this environment.
    pub(super) fn get_type(&self, id: TypeId) -> &Type {
        &self.type_id_to_type[id]
    }

    /// Given an identifier, get the associated [`TypeId`].
    pub(super) fn get_type_id_for_ident(&mut self, ident: &String) -> TypeId {
        let maybe_res = self.ident_to_type_id.get(ident).copied();

        match maybe_res {
            Some(res) => res,

            None => {
                let type_id = self.add_new_type_variable();
                self.ident_to_type_id.insert(ident.clone(), type_id);
                type_id
            },
        }
    }

    /// Unify two types, asserting that they must be equal.
    pub(super) fn unify(
        &mut self,
        first_id: TypeId,
        second_id: TypeId,
        span: Span,
    ) -> Result<(), ()> {
        let first_ty = self.get_type(first_id);
        let second_ty = self.get_type(second_id);
        match (first_ty, second_ty) {
            (Type::Variable(_), _) => {
                if first_ty != second_ty {
                    self.type_id_to_type[first_id] = self.get_type(second_id).clone();
                }
            },

            (_, Type::Variable(_)) => {
                self.unify(second_id, first_id, span)?;
            },

            (Type::Integer, Type::Integer)
            | (Type::Boolean, Type::Boolean)
            | (Type::Float, Type::Float)
            | (Type::String, Type::String) => {},

            (Type::Function(params_id_1, ret_id_1), Type::Function(params_id_2, ret_id_2)) => {
                // These variables are needed to copy the type ids.
                // Otherwise, the borrow checker complains when `unify`
                // tries to take a mutable reference to `self`.
                let params_id_1 = *params_id_1;
                let params_id_2 = *params_id_2;
                let ret_id_1 = *ret_id_1;
                let ret_id_2 = *ret_id_2;

                self.unify(params_id_1, params_id_2, span)?;
                self.unify(ret_id_1, ret_id_2, span)?;
            },

            (Type::Tuple(type_ids_1), Type::Tuple(type_ids_2)) => {
                // Sort of a hack: the type ids are iterated over, copied, and zipped together
                // into tuples. These tuples are then collected into a Vec, which then gets
                // iterated over. This doesn't seem great, but iterating over the iterator
                // produced by `zip` produced a borrow checker error.
                let type_ids_2_iter = type_ids_2.iter().copied();
                let type_ids: Vec<_> = type_ids_1.iter().copied().zip(type_ids_2_iter).collect();

                for (left_id, right_id) in type_ids {
                    self.unify(left_id, right_id, span)?;
                }
            },

            (_, _) => {
                self.error_ctx
                    .build_error(format!(
                        "expected {}, got {}",
                        second_ty.to_string(self),
                        first_ty.to_string(self)
                    ))
                    .span_label(span, "type error detected here")
                    .note("this may be inaccurate; these diagnostics will improve in the future")
                    .emit();

                return Err(());
            },
        }

        Ok(())
    }
}
