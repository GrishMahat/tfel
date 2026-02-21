use tfel::environment::Environment;
use tfel::evaluator::Value;

#[test]
fn nested_scope_falls_back_to_parent() {
    let root = Environment::new();
    root.define("x", Value::Number(42.0));

    let child = Environment::new_enclosed(root.clone());
    assert_eq!(child.get("x"), Some(Value::Number(42.0)));

    child.define("x", Value::Number(1.0));
    assert_eq!(child.get("x"), Some(Value::Number(1.0)));
    assert_eq!(root.get("x"), Some(Value::Number(42.0)));
}

#[test]
fn visible_names_include_parent_and_child_without_duplicates() {
    let root = Environment::new();
    root.define("shared", Value::Number(1.0));
    root.define("root", Value::Number(2.0));

    let child = Environment::new_enclosed(root);
    child.define("shared", Value::Number(3.0));
    child.define("child", Value::Number(4.0));

    let names = child.visible_names();
    assert_eq!(names, vec!["child", "root", "shared"]);
}
