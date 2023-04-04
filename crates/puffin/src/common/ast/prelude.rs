pub use super::Expr;
pub use super::Ident;
pub use super::Literal;
pub use super::Pat;
pub use super::Path;
pub use super::Stmt;
pub use super::TestCmp;
pub use puffin_macro::ast;

impl<T: TestCmp> TestCmp for Vec<T> {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        if self.len() != b.len() {
            return false;
        }
        for i in 0..self.len() {
            if let Some(vec1) = self.get(i) {
                if let Some(vec2) = b.get(i) {
                    if !vec1.test_ast_cmp(vec2) {
                        return false;
                    }
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }
}

impl<K: TestCmp, V: TestCmp> TestCmp for ahash::AHashMap<K, V> {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        // Get entry and value pairs
        let self_entries = self.iter().collect::<Vec<(&K, &V)>>();
        let mut b_entries = b.iter().collect::<Vec<(&K, &V)>>();

        // Ensure they are the same length
        if self_entries.len() != b_entries.len() {
            return false;
        }

        for self_ent in self_entries {
            let mut found = false;
            // Test against each pair in b
            for (i, b_ent) in b_entries.iter().enumerate() {
                if self_ent.0.test_ast_cmp(b_ent.0) && self_ent.1.test_ast_cmp(b_ent.1) {
                    // If they match remove the pair from b
                    found = true;
                    b_entries.remove(i);
                    break;
                }
            }
            // If a match isn't found it isn't equal
            if !found {
                return false;
            }
        }
        true
    }
}

impl<T: TestCmp> TestCmp for Option<T> {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        if let Some(t1) = self {
            if let Some(t2) = b {
                return t1.test_ast_cmp(t2);
            }
        } else if b.is_none() {
            return true;
        }
        false
    }
}

impl<A: TestCmp, B: TestCmp> TestCmp for Vec<(A, B)> {
    fn test_ast_cmp(&self, b: &Self) -> bool {
        if self.len() != b.len() {
            return false;
        }
        for i in 0..self.len() {
            if let Some(vec1) = self.get(i) {
                if let Some(vec2) = b.get(i) {
                    if !(vec1.0.test_ast_cmp(&vec2.0) && vec1.1.test_ast_cmp(&vec2.1)) {
                        return false;
                    }
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }
}
