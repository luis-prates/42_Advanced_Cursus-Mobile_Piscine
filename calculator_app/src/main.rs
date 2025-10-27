use dioxus::logger::tracing;
use dioxus::prelude::*;
mod calc;
use calc::eval_expression;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");

#[derive(PartialEq, Props, Clone)]
struct ResultProps {
    #[props(default = "0".to_string())]
    result: String,
    #[props(default = "0".to_string())]
    expression: String,
}

fn main() {
    dioxus::launch(App);
}

fn is_operator_char(c: char) -> bool {
    matches!(c, '+' | '-' | 'x' | 'X' | '*' | '/' | '÷')
}

#[component]
fn App() -> Element {
    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Stylesheet { href: MAIN_CSS }
        AppBar {}
        Calculator {}
    }
}

#[component]
fn Calculator() -> Element {
    let calc_pad = [
        "7", "8", "9", "C", "AC", "4", "5", "6", "+", "-", "1", "2", "3", "x", "/", "0", ".", "00",
        "=",
    ];

    // UI state as single expression string and result
    let mut expr = use_signal(|| "0".to_string());
    let mut result = use_signal(|| "0".to_string());
    let mut just_evaluated = use_signal(|| false);
    let mut has_error = use_signal(|| false);

    rsx! {
        div { class: "calculator",
            div { class: "text-field-container",
                DisplayField {
                    result: result().to_string(),
                    expression: expr().to_string(),
                }
            }
            div { id: "buttons",
                for value in calc_pad {
                    match value {
                        // Numeric buttons
                        v if v.parse::<i32>().is_ok() => rsx! {
                            button {
                                class: "grid-item number",
                                onclick: move |_| {
                                    // If we just evaluated and the user types a number -> start new expression
                                    if just_evaluated() {
                                        expr.set(v.to_string());
                                        just_evaluated.set(false);
                                        return;
                                    }
                                    let s = expr();
                                    if s.as_str() == "0" {
                                        // Avoid multiple leading zeros
                                        if v == "0" || v == "00" {
                                            return;
                                        } else {
                                            expr.set(v.to_string());
                                            return;
                                        }
                                    }
                                    expr.set(format!("{}{}", s, v));
                                },
                                "{value}"
                            }
                        },
                        // Decimal point
                        "." => rsx! {
                            button {
                                class: "grid-item number",
                                onclick: move |_| {
                                    if just_evaluated() {
                                        expr.set("0.".to_string());
                                        just_evaluated.set(false);
                                        return;
                                    }
                                    let s = expr();
                                    // find current number segment after last operator
                                    let last_op = s.rfind(|c: char| is_operator_char(c));
                                    let start = match last_op { Some(i) => i + 1, None => 0 };
                                    let current = &s[start..];
                                    if !current.contains('.') {
                                        if s.as_str() == "0" {
                                            expr.set("0.".to_string());
                                        } else if current.is_empty() {
                                            expr.set(format!("{}0.", s));
                                        } else {
                                            expr.set(format!("{}.", s));
                                        }
                                    }
                                },
                                "{value}"
                            }
                        },
                        // Backspace / Clear
                        "C" => rsx! {
                            button {
                                class: "grid-item reset",
                                onclick: move |_| {
                                    if just_evaluated() {
                                        expr.set("0".to_string());
                                        just_evaluated.set(false);
                                        return;
                                    }
                                    let mut s = expr();
                                    if s.len() <= 1 {
                                        expr.set("0".to_string());
                                        return;
                                    }
                                    // If last char is operator, remove it
                                    if s.chars().last().map(is_operator_char).unwrap_or(false) {
                                        s.pop();
                                        expr.set(s);
                                        return;
                                    }
                                    // Otherwise pop one char
                                    s.pop();
                                    if s.is_empty() || s.as_str() == "-" {
                                        expr.set("0".to_string());
                                    } else {
                                        expr.set(s);
                                    }
                                },
                                "{value}"
                            }
                        },
                        // All Clear
                        "AC" => rsx! {
                            button {
                                class: "grid-item reset",
                                onclick: move |_| {
                                    expr.set("0".to_string());
                                    result.set("0".to_string());
                                    just_evaluated.set(false);
                                },
                                "{value}"
                            }
                        },
                        // Operators
                        "+" | "x" | "-" | "/" => rsx! {
                            button {
                                class: "grid-item operator",
                                onclick: move |_| {
                                    let op = value.chars().next().unwrap();
                                    // If we just evaluated, begin new expression from result
                                    if just_evaluated() {
                                        if has_error() {
                                            // if there was an error, start fresh
                                            expr.set(format!("0{}", op));
                                            has_error.set(false);
                                            just_evaluated.set(false);
                                            return;
                                        } else {
                                            let base = result();
                                            expr.set(format!("{}{}", base, op));
                                            just_evaluated.set(false);
                                            return;
                                        }
                                    }
                                    let mut s = expr();
                                    // allow unary minus at start
                                    if s.as_str() == "0" && op == '-' {
                                        expr.set("-".to_string());
                                        return;
                                    }
                                    // if last char is an operator, replace it
                                    if s.chars().last().map(is_operator_char).unwrap_or(false) {
                                        s.pop();
                                        s.push(op);
                                        expr.set(s);
                                        return;
                                    }
                                    // normal append
                                    if s.as_str() == "0" {
                                        // "0" followed by operator: keep 0 as LHS
                                        expr.set(format!("0{}", op));
                                    } else {
                                        expr.set(format!("{}{}", s, op));
                                    }
                                },
                                "{value}"
                            }
                        },
                        // Equals: evaluate with precedence
                        "=" => rsx! {
                            button {
                                class: "grid-item operator",
                                onclick: move |_| {
                                    // Prepare expression: trim trailing operators
                                    let mut s = expr();
                                    while s.chars().last().map(is_operator_char).unwrap_or(false) {
                                        s.pop();
                                    }
                                    tracing::debug!("Evaluating expression: {}", s);
                                    match eval_expression(&s) {
                                        Ok(v) => {
                                            let out = v.to_string();
                                            result.set(out.clone());
                                            expr.set(out.clone());
                                            just_evaluated.set(true);
                                        }
                                        Err(e) => {
                                            result.set(e.to_string());
                                            just_evaluated.set(true);
                                            has_error.set(true);
                                        }
                                    }
                                },
                                "{value}"
                            }
                        },
                        _ => rsx!(button { class: "grid-item", onclick: move |_| {}, "{value}" })
                    }
                }
            }
        }
    }
}

#[component]
fn AppBar() -> Element {
    rsx! {
        nav { class: "app-bar",
            div { class: "app-bar__title", "Calculator" }
        }
    }
}

#[component]
fn DisplayField(props: ResultProps) -> Element {
    rsx! {
        div { class: "display-area",
            div { class: "text-display expression", "{props.expression}" }
            div { class: "text-display result", "{props.result}" }
        }
    }
}
