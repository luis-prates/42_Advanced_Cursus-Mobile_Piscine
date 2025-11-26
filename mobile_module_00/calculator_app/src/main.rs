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
    // Button layout
    let calc_pad = [
        "7", "8", "9", "C", "AC", "4", "5", "6", "+", "-", "1", "2", "3", "x", "/", "0", ".", "00",
        "=",
    ];

    // UI state: single expression string, last result, whether the last action was "="
    let mut expr_sig = use_signal(|| "0".to_string());
    let mut result_sig = use_signal(|| "0".to_string());
    let mut just_eval_sig = use_signal(|| false);

    // Helper factories: return onclick closures that capture cloned signals.
    // Using factories avoids duplicating the same signal-cloning logic for each button.

    let make_digit_handler = |d: &'static str| {
        let mut expr = expr_sig.clone();
        let mut result = result_sig.clone();
        let mut just_eval = just_eval_sig.clone();
        move |_| {
            // If the last action was evaluation, typing a digit starts a new expression.
            if just_eval() {
                expr.set(d.to_string());
                just_eval.set(false);
                // keep `result` for potential operator chaining
                return;
            }

            let s = expr();
            if s.as_str() == "0" {
                // Avoid creating many leading zeros; respect "00" input but don't chain zeros.
                if d == "0" || d == "00" {
                    return;
                } else {
                    expr.set(d.to_string());
                    return;
                }
            }
            expr.set(format!("{}{}", s, d));
        }
    };

    let make_decimal_handler = || {
        let mut expr = expr_sig.clone();
        let mut just_eval = just_eval_sig.clone();
        move |_| {
            if just_eval() {
                expr.set("0.".to_string());
                just_eval.set(false);
                return;
            }
            let s = expr();
            let last_op = s.rfind(|c: char| is_operator_char(c));
            let start = match last_op {
                Some(i) => i + 1,
                None => 0,
            };
            let current = &s[start..];
            if !current.contains('.') {
                if s.as_str() == "0" {
                    expr.set("0.".to_string());
                } else {
                    expr.set(format!("{}.", s));
                }
            }
        }
    };

    let make_backspace_handler = || {
        let mut expr = expr_sig.clone();
        let mut just_eval = just_eval_sig.clone();
        move |_| {
            if just_eval() {
                expr.set("0".to_string());
                just_eval.set(false);
                return;
            }
            let mut s = expr();
            if s.len() <= 1 {
                expr.set("0".to_string());
                return;
            }
            if s.chars().last().map(is_operator_char).unwrap_or(false) {
                s.pop();
                expr.set(s);
                return;
            }
            s.pop();
            if s.is_empty() || s.as_str() == "-" {
                expr.set("0".to_string());
            } else {
                expr.set(s);
            }
        }
    };

    let make_all_clear_handler = || {
        let mut expr = expr_sig.clone();
        let mut result = result_sig.clone();
        let mut just_eval = just_eval_sig.clone();
        move |_| {
            expr.set("0".to_string());
            result.set("0".to_string());
            just_eval.set(false);
        }
    };

    let make_operator_handler = |op_chr: char| {
        let mut expr = expr_sig.clone();
        let mut result = result_sig.clone();
        let mut just_eval = just_eval_sig.clone();
        move |_| {
            // If last action was evaluation, start new expression from the numeric result
            if just_eval() {
                let base = result();
                expr.set(format!("{}{}", base, op_chr));
                just_eval.set(false);
                return;
            }

            let mut s = expr();
            // Allow unary minus at start
            if s.as_str() == "0" && op_chr == '-' {
                expr.set("-".to_string());
                return;
            }
            // If last char is operator, replace it
            if s.chars().last().map(is_operator_char).unwrap_or(false) {
                s.pop();
                s.push(op_chr);
                expr.set(s);
                return;
            }
            // Normal append
            if s.as_str() == "0" {
                expr.set(format!("0{}", op_chr));
            } else {
                expr.set(format!("{}{}", s, op_chr));
            }
        }
    };

    let make_equals_handler = || {
        let mut expr = expr_sig.clone();
        let mut result = result_sig.clone();
        let mut just_eval = just_eval_sig.clone();
        move |_| {
            let mut s = expr();
            // Trim trailing operators
            while s.chars().last().map(is_operator_char).unwrap_or(false) {
                s.pop();
            }
            match eval_expression(&s) {
                Ok(v) => {
                    let out = v.to_string();
                    result.set(out.clone());
                    expr.set(out);
                    just_eval.set(true);
                }
                Err(e) => {
                    result.set(e.to_string());
                    just_eval.set(true);
                }
            }
        }
    };

    // Build UI: use factories to provide onclick handlers
    rsx! {
        div { class: "calculator",
            div { class: "text-field-container",
                DisplayField {
                    result: result_sig().to_string(),
                    expression: expr_sig().to_string(),
                }
            }
            div { id: "buttons",
                for value in calc_pad {
                    match value {
                        // Digit buttons
                        v if v.parse::<i32>().is_ok() => rsx! {
                            button {
                                class: "grid-item number",
                                onclick: make_digit_handler(v),
                                "{value}"
                            }
                        },
                        "." => rsx! {
                            button {
                                class: "grid-item number",
                                onclick: make_decimal_handler(),
                                "{value}"
                            }
                        },
                        "C" => rsx! {
                            button {
                                class: "grid-item reset",
                                onclick: make_backspace_handler(),
                                "{value}"
                            }
                        },
                        "AC" => rsx! {
                            button {
                                class: "grid-item reset",
                                onclick: make_all_clear_handler(),
                                "{value}"
                            }
                        },
                        "+" | "x" | "-" | "/" => rsx! {
                            button {
                                class: "grid-item operator",
                                onclick: make_operator_handler(value.chars().next().unwrap()),
                                "{value}"
                            }
                        },
                        "=" => rsx! {
                            button {
                                class: "grid-item operator",
                                onclick: make_equals_handler(),
                                "{value}"
                            }
                        },
                        // Fallback (shouldn't happen)
                        _ => rsx! {
                            button { class: "grid-item", onclick: move |_| {}, "{value}" }
                        }
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
            div { class: "app-bar__title", "My App" }
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
