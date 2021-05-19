use std::collections::HashMap;

use crate::{
    errors::Span,
    parser::ast::{Path, PathSearchLocation, PathSegment},
};

#[derive(Debug)]
pub(super) struct Scope {
    absolute_path: Path,
    contents: HashMap<PathSegment, ScopeMember>,
}

impl Scope {
    pub(super) fn new(absolute_path: Path) -> Self {
        debug_assert!(matches!(
            absolute_path.location,
            PathSearchLocation::Absolute
        ));

        Self {
            absolute_path,
            contents: HashMap::new(),
        }
    }

    /// Convenience function for accessing the underlying [`HashMap`].
    /// See [`HashMap::get`] for more information.
    pub(super) fn get(&self, key: &PathSegment) -> Option<&ScopeMember> {
        self.contents.get(key)
    }

    /// Convenience function for accessing the underlying [`HashMap`].
    /// See [`HashMap::get_mut`] for more information.
    pub(super) fn get_mut(&mut self, key: &PathSegment) -> Option<&mut ScopeMember> {
        self.contents.get_mut(key)
    }

    /// Convenience function for accessing the underlying [`HashMap`].
    /// See [`HashMap::insert`] for more information.
    pub(super) fn insert(&mut self, key: PathSegment, value: ScopeMember) -> Option<ScopeMember> {
        self.contents.insert(key, value)
    }
}

#[derive(Debug)]
pub(super) enum ScopeMember {
    Scope(Scope),
    Leaf(Path),
}

impl ScopeMember {
    fn path(&self) -> &Path {
        match self {
            ScopeMember::Scope(scope) => &scope.absolute_path,
            ScopeMember::Leaf(path) => path,
        }
    }

    fn matches(&self, segment: &PathSegment) -> bool {
        self.path().unwrap_last_segment() == segment
    }
}

#[derive(Debug)]
pub(super) struct Scopes {
    contents: Scope,
    /// The current scope being built.
    /// Used for resolution of relative imports.
    pub(super) current_path: Path,
}

impl Scopes {
    pub(super) fn new() -> Self {
        let current_path = Path::new(PathSearchLocation::Absolute, Vec::new(), Span::empty());

        Self {
            contents: Scope::new(current_path.clone()),
            current_path,
        }
    }

    /// Pushes a scope identifier to the current path being resolved.
    pub(super) fn push_scope(&mut self, segment: PathSegment) {
        let parent = self.current_path.clone();
        let new_path = self.current_path.create_child(segment.clone());
        self.current_path = new_path.clone();

        let parent_scope = match self.scope_for_mut(&parent) {
            Some(res) => res,
            None => &mut self.contents,
        };

        let scope = ScopeMember::Scope(Scope::new(new_path));
        parent_scope.insert(segment, scope);
    }

    /// Pops a scope identifier from the path being resolved.
    /// Returns whether or not we've just popped the last scope
    pub(super) fn pop_scope(&mut self) -> bool {
        self.current_path.segments.pop();
        self.current_path.segments.is_empty()
    }

    /// Adds a local name to the current scope.
    pub(super) fn add_local(&mut self, segment: PathSegment) -> Result<Path, ()> {
        let path = self.current_path.create_child(segment.clone());
        let res = self.current_scope_mut();
        res.insert(segment, ScopeMember::Scope(Scope::new(path.clone())));
        Ok(path)
    }

    /// Given a path to a scope, get the corresponding scope.
    /// For example, to get the scope that `std::mem::forget` lives
    /// in, you'd use `scope_for` with a path that represents `std::mem`.
    /// Note that upon encountering a leaf, this will return `None`.
    /// For instance, using `scope_for` on a path that represents
    /// `std::mem::forget` would return `None`.
    pub(super) fn scope_for(&self, path: &Path) -> Option<&Scope> {
        match path.location {
            PathSearchLocation::Root => todo!(),
            PathSearchLocation::Super => todo!(),
            PathSearchLocation::SelfMod => todo!(),

            PathSearchLocation::Absolute => {
                let mut current_scope = &self.contents;

                for segment in &path.segments {
                    match current_scope.get(&segment) {
                        Some(ScopeMember::Scope(scope)) => {
                            current_scope = scope;
                        },

                        x => {
                            return None;
                        },
                    }
                }
                Some(current_scope)
            },

            PathSearchLocation::Local => {
                let mut search_path = self.current_path.clone();

                while !search_path.segments.is_empty() {
                    let maybe_this_path = search_path.append_path(path);

                    match self.scope_for(&maybe_this_path) {
                        Some(res) => {
                            return Some(res);
                        },

                        None => {
                            search_path.segments.pop();
                        },
                    }
                }
                None
            },
        }
    }

    /// Given a path to a scope, get a mutable reference
    /// to the corresponding scope.
    /// For example, to get the scope that `std::mem::forget` lives
    /// in, you'd use `scope_for` with a path that represents `std::mem`.
    /// Note that upon encountering a leaf, this will return `None`.
    /// For instance, using `scope_for` on a path that represents
    /// `std::mem::forget` would return `None`.
    pub(super) fn scope_for_mut(&mut self, path: &Path) -> Option<&mut Scope> {
        match path.location {
            PathSearchLocation::Root => todo!(),
            PathSearchLocation::Super => todo!(),
            PathSearchLocation::SelfMod => todo!(),

            PathSearchLocation::Absolute => {
                let mut current_scope = &mut self.contents;

                for segment in &path.segments {
                    match current_scope.get_mut(&segment) {
                        Some(ScopeMember::Scope(scope)) => {
                            current_scope = scope;
                        },

                        _ => return None,
                    }
                }

                Some(current_scope)
            },

            PathSearchLocation::Local => {
                let absolute_path = self.current_path.append_path(path);
                self.scope_for_mut(&absolute_path)
            },
        }
    }

    pub(super) fn current_scope(&self) -> &Scope {
        self.scope_for(&self.current_path)
            .expect("`Scopes` should always have a scope")
    }

    pub(super) fn current_scope_mut(&mut self) -> &mut Scope {
        self.scope_for_mut(&self.current_path.clone())
            .expect("`Scopes` should always have a scope")
    }

    pub(super) fn lookup(&mut self, path: &Path) -> Option<Path> {
        match path.location {
            PathSearchLocation::Root => todo!("root lookup"),
            PathSearchLocation::Super => todo!("super lookup"),
            PathSearchLocation::SelfMod => todo!("self lookup"),

            PathSearchLocation::Absolute => {
                // TODO: actually look up and validate the path
                Some(path.clone())
            },

            PathSearchLocation::Local => {
                let mut search_path = self.current_path.clone();
                let this_path_last = path.unwrap_last_segment();

                while !search_path.segments.is_empty() {
                    let maybe_this_path = search_path.append_path(path);

                    match self.scope_for(&maybe_this_path) {
                        Some(res) => {
                            return Some(res.absolute_path.clone());
                        },

                        None => {
                            search_path.segments.pop();
                        },
                    }
                }

                None
            },
        }
    }
}
