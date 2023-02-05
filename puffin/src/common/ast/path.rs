use super::Ident;

/// A path to a module in puffin
#[puffin_macro::ast(_)]
#[derive(Debug, PartialEq)]
pub struct Path {
    /// The path as a vec of literals
    path: Vec<Ident>,
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.path
                .iter()
                .map(|l| format!("{}::", l))
                .collect::<String>()
                .trim_end_matches("::")
        )
    }
}
