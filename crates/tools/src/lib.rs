use heck::{ToShoutySnakeCase, ToSnakeCase, ToUpperCamelCase};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};
use tera::Value;

// Whether the tool should verify the generated files or update them
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    Verify,
    Update,
}

/// Used to provide sequential IDs to the std items;
static STD_COUNTER: AtomicU32 = AtomicU32::new(0);

/// Path to ron file that supplies Puffin's grammer
pub const GRAMMER: &str = "crates/templates/grammer.ron";
/// Path to template file for syntax kind. Format taken from [Mun](https://github.com/mun-lang/mun)
pub const SYNTAX_KIND: &str = "crates/templates/syntax_kind.rs.tera";
/// Path to location of generated syntax kind file
pub const SYNTAX_KIND_TARGET: &str = "crates/puffin_ast/src/syntax_kind.rs";

/// Path to the std impls ron file
pub const STD: &str = "crates/templates/std.ron";
/// Path to the std impls template file
pub const STD_IMPLS_TEMPLATE: &str = "crates/templates/impls.rs.tera";
/// Path to the std impls target
pub const STD_IMPLS_TARGET: &str = "crates/puffin_hir/src/std/impl_std.rs";

/// Creates a tera instance with the provided filters and functions. Taken from [Mun](https://github.com/mun-lang/mun).
fn create_tera() -> tera::Tera {
    let mut res = tera::Tera::default();
    res.register_filter("camel", camel);
    res.register_filter("snake", snake);
    res.register_filter("SCREAM", scream);
    res.register_function("concat", |args: &HashMap<String, Value>| {
        let mut elements = Vec::new();
        for &key in ["a", "b", "c"].iter() {
            let val = match args.get(key) {
                Some(val) => val,
                None => continue,
            };
            let val = val.as_array().unwrap();
            elements.extend(val.iter().cloned());
        }
        Ok(Value::Array(elements))
    });
    res.register_function("next_id", |_: &HashMap<String, Value>| {
        Ok(Value::Number(
            STD_COUNTER.fetch_add(1, Ordering::SeqCst).into(),
        ))
    });

    return res;

    /// Convert value to CamelCase
    fn camel(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
        Ok(value.as_str().unwrap().to_upper_camel_case().into())
    }

    /// Convert value to snake_case
    fn snake(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
        Ok(value.as_str().unwrap().to_snake_case().into())
    }

    /// Convert value to SCREAM_CASE
    fn scream(value: &Value, _: &HashMap<String, Value>) -> tera::Result<Value> {
        Ok(value.as_str().unwrap().to_shouty_snake_case().into())
    }
}

/// Generate the files
pub fn generate(mode: Mode) -> anyhow::Result<()> {
    let grammer = project_root().join(GRAMMER);
    let syntax_kind = project_root().join(SYNTAX_KIND);
    let syntax_kind_target = project_root().join(Path::new(SYNTAX_KIND_TARGET));
    generate_from_template(&syntax_kind, &grammer, &syntax_kind_target, mode)
}

pub fn std(mode: Mode) -> anyhow::Result<()> {
    let std = project_root().join(STD);
    let std_impls = project_root().join(STD_IMPLS_TEMPLATE);
    let std_impls_target = project_root().join(Path::new(STD_IMPLS_TARGET));
    generate_from_template(&std_impls, &std, &std_impls_target, mode)
}

/// Generates a file from the template
fn generate_from_template(
    template: &Path,
    src: &Path,
    target: &Path,
    mode: Mode,
) -> anyhow::Result<()> {
    let template = std::fs::read_to_string(template)?;
    let src: ron::Value = {
        let text = std::fs::read_to_string(src)?;
        ron::de::from_str(&text)?
    };
    let mut tera = create_tera();
    tera.add_raw_template("_src", &template)?;
    let content = tera.render("_src", &tera::Context::from_serialize(src)?)?;
    update(target, &content, mode)
}

/// Updates the file or reports differences depending on which mode is provided
fn update(path: &Path, new_contents: &str, mode: Mode) -> anyhow::Result<()> {
    let old_contents = std::fs::read_to_string(path)?.replace("\r\n", "\n");
    let new_contents = new_contents.replace("\r\n", "\n");

    // No changes to report so we simply exit
    if old_contents == new_contents {
        return Ok(());
    }

    if mode == Mode::Verify {
        // If we are verifying, report the changes
        let changes = similar::TextDiff::from_lines(&old_contents, &new_contents);

        for change in changes.iter_all_changes() {
            let sign = match change.tag() {
                similar::ChangeTag::Delete => "-",
                similar::ChangeTag::Insert => "+",
                similar::ChangeTag::Equal => " ",
            };
            print!("{}| {}", sign, change);
        }
        anyhow::bail!("There were differences in the files")
    }
    // If we are updating, update the file
    eprintln!("updating {}", path.display());
    std::fs::write(path, new_contents)?;
    Ok(())
}

/// Provides the project root
fn project_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .unwrap()
        .to_path_buf()
}

#[cfg(test)]
mod template_test {
    use crate::Mode;

    #[test]
    fn grammer_is_fresh() {
        if let Err(e) = super::generate(Mode::Verify) {
            panic!("Please update syntax by running cargo gen-syntax as it's out of date\n{e}")
        }
    }

    #[test]
    fn std_is_fresh() {
        if let Err(e) = super::std(Mode::Verify) {
            panic!("Please update std by running cargo gen-std as it's out of date\n{e}")
        }
    }
}
