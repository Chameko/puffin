use crate::{id::ExprID, model::body::ComptimeBody};

/// A resolved comptime block with its body and the expression it resolves into
pub struct ResolvedComptime {
    pub body: ComptimeBody,
    pub ret: ExprID,
}
