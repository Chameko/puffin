/// Describes the visibility of an item
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility {
    /// Can be seen and used by all modules
    Public,
    /// Can only be seen and used by the containing module
    Private,
    /// Can be seen and used by all modules in puffin, but is invisible to the embeddor
    Local,
}
