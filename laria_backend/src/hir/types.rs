use std::collections::HashMap;

use super::hir_tree::Path;
use crate::errors::Span;

pub(super) type TypeId = usize;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum Type {
    Variable(u64),
    Integer,
    Boolean,
    Float,
    String,
    /// The never type, `!`. Conceptually, this is
    /// the [bottom type].
    ///
    /// [bottom type]: https://en.wikipedia.org/wiki/Bottom_type
    Never,
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
    pub(super) fn to_string(&self, ty_env: &TypeEnvironment) -> String {
        match self {
            Type::Variable(id) => format!("{{type variable #{}}}", id),
            Type::Integer => "{integer}".to_owned(),
            Type::Boolean => "bool".to_owned(),
            Type::Float => "{float}".to_owned(),
            Type::String => "string".to_owned(),
            Type::Never => "!".to_owned(),

            Type::Function(args_id, return_id) => {
                let args_ty = ty_env.type_from_id(*args_id).to_string(ty_env);
                let ret_ty = ty_env.type_from_id(*return_id).to_string(ty_env);
                format!("{} -> {}", args_ty, ret_ty)
            },

            Type::Tuple(contents) => {
                let contents_string = contents
                    .iter()
                    .map(|id| ty_env.type_from_id(*id).to_string(ty_env))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({})", contents_string)
            },
        }
    }
}

/// Holds information on the type universe.
#[derive(Debug)]
pub(super) struct TypeEnvironment {
    type_id_to_type: Vec<Type>,
    // Let's assume name resolution was done already,
    // so we don't have any invalid identifiers
    // TODO: not likely; consider switching to something
    // like Rust's `DefId`, which is more or less an
    // interned path to the ident
    path_to_type_id: HashMap<Path, TypeId>,
    next_type_var_id: u64,
}

impl TypeEnvironment {
    pub(super) fn new() -> Self {
        // Please keep in sync with primitive paths below
        let type_id_to_type = vec![
            Type::Integer,
            Type::Boolean,
            Type::Float,
            Type::String,
            Type::Never,
            Type::unit(),
        ];

        fn path_for(segment: impl Into<String>) -> Path {
            Path::new(
                vec!["root".to_owned(), segment.into()],
                Span::empty(),
                false,
            )
        }

        let mut path_to_type_id = HashMap::new();
        // Bring in the primitives
        // Please keep in sync with type_id_to_type above
        // TODO: hack?
        path_to_type_id.insert(path_for("i64"), 0);
        path_to_type_id.insert(path_for("bool"), 1);
        path_to_type_id.insert(path_for("f64"), 2);
        path_to_type_id.insert(path_for("string"), 3);

        Self {
            type_id_to_type,
            path_to_type_id,
            next_type_var_id: 0,
        }
    }

    /// Adds the given type to the universe.
    /// Returns the type's [`TypeId`].
    pub(super) fn add_type(&mut self, ty: Type) -> TypeId {
        self.type_id_to_type.push(ty);
        self.type_id_to_type.len() - 1
    }

    /// Adds a new type variable to the universe with a unique variable id.
    /// Returns the variable's [`TypeId`] (not its variable id).
    pub(super) fn add_new_type_variable(&mut self) -> TypeId {
        let ty = Type::Variable(self.next_type_var_id);
        self.next_type_var_id += 1;
        self.add_type(ty)
    }

    /// Given a [`TypeId`], get the concrete type from this environment.
    pub(super) fn type_from_id(&self, id: TypeId) -> &Type {
        &self.type_id_to_type[id]
    }

    /// Given a path, get the associated [`TypeId`].
    pub(super) fn type_id_for_path(&mut self, path: &Path) -> TypeId {
        let maybe_res = self.path_to_type_id.get(path).copied();

        match maybe_res {
            Some(res) => res,

            None => {
                let type_id = self.add_new_type_variable();
                self.path_to_type_id.insert(path.clone(), type_id);
                type_id
            },
        }
    }

    /// Unify two types, asserting that they must be equal.
    pub(super) fn unify(&mut self, first_id: TypeId, second_id: TypeId) -> Result<(), ()> {
        let first_ty = self.type_from_id(first_id);
        let second_ty = self.type_from_id(second_id);
        match (first_ty, second_ty) {
            (Type::Variable(_), _) => {
                if first_ty != second_ty {
                    self.type_id_to_type[first_id] = self.type_from_id(second_id).clone();
                }
            },

            (_, Type::Variable(_)) => {
                self.unify(second_id, first_id)?;
            },

            (Type::Integer, Type::Integer)
            | (Type::Boolean, Type::Boolean)
            | (Type::Float, Type::Float)
            | (Type::String, Type::String) => {},

            // `!` subtypes everything, so any type should unify with `!`
            (Type::Never, _) => self.type_id_to_type[second_id] = Type::Never,

            (Type::Function(params_id_1, ret_id_1), Type::Function(params_id_2, ret_id_2)) => {
                // These variables are needed to copy the type ids.
                // Otherwise, the borrow checker complains when `unify`
                // tries to take a mutable reference to `self`.
                let params_id_1 = *params_id_1;
                let params_id_2 = *params_id_2;
                let ret_id_1 = *ret_id_1;
                let ret_id_2 = *ret_id_2;

                self.unify(params_id_1, params_id_2)?;
                self.unify(ret_id_1, ret_id_2)?;
            },

            (Type::Tuple(type_ids_1), Type::Tuple(type_ids_2))
                if type_ids_1.len() == type_ids_2.len() =>
            {
                // Sort of a hack: the type ids are iterated over, copied, and zipped together
                // into tuples. These tuples are then collected into a Vec, which then gets
                // iterated over. This doesn't seem great, but iterating over the iterator
                // produced by `zip` produced a borrow checker error.
                let type_ids_2_iter = type_ids_2.iter().copied();
                let type_ids: Vec<_> = type_ids_1.iter().copied().zip(type_ids_2_iter).collect();

                for (left_id, right_id) in type_ids {
                    self.unify(left_id, right_id)?;
                }
            }

            (_, _) => {
                return Err(());
            },
        }

        Ok(())
    }
}
