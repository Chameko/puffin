use super::Ident;
use super::TestCmp;

/// A path to a module in puffin
#[puffin_macro::ast(_)]
#[derive(Debug, PartialEq, Clone)]
pub struct Path {
    /// The path as a vec of literals
    path: Vec<Ident>,
}

impl TestCmp for Path {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        self.path.test_ast_cmp(&b.path)
    }
}

impl Path {
    pub fn test_node(path: Vec<Ident>) -> Self {
        Self { path, range: 0..=0 }
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.path
                .iter()
                .map(|l| format!("{l}::"))
                .collect::<String>()
                .trim_end_matches("::")
        )
    }
}
