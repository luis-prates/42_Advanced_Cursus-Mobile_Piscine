use dioxus::{
    logger::tracing::{self, info},
    prelude::*,
};
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{One, Zero};

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const HEADER_SVG: Asset = asset!("/assets/header.svg");

#[derive(Clone, Debug)]
pub enum Value {
    Int(BigInt),
    Rat(BigRational),
}

use std::fmt;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{i}"),
            Value::Rat(r) => {
                if *r.denom() == BigInt::one() {
                    write!(f, "{}", r.numer())
                } else {
                    write!(f, "{r}")
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum CalcError {
    InvalidNumber(String),      // e.g. failed BigInt::parse_bytes
    UnsupportedDecimal(String), // if you don’t yet handle “1.23”
    DivideByZero,               // e.g. division by zero
    ParseError(String),         // e.g. mismatched parentheses or unexpected token
}

impl fmt::Display for CalcError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CalcError::InvalidNumber(s) => write!(f, "invalid number: '{s}'"),
            CalcError::UnsupportedDecimal(s) => {
                write!(f, "decimal numbers not supported yet: '{s}'")
            }
            CalcError::DivideByZero => write!(f, "division by zero"),
            CalcError::ParseError(s) => write!(f, "parse error: {s}"),
        }
    }
}

impl std::error::Error for CalcError {}

/// The four basic binary operators (you can extend this with Mod, Pow, etc.)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Op {
    /// Try to convert a single‑char token into an `Op`
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '+' => Some(Op::Add),
            '-' => Some(Op::Sub),
            '*' | 'x' => Some(Op::Mul),
            '/' | '÷' => Some(Op::Div),
            _ => None,
        }
    }

    /// Precedence level for use in parsing (higher = binds tighter)
    pub fn precedence(self) -> u8 {
        match self {
            Op::Add | Op::Sub => 1,
            Op::Mul | Op::Div => 2,
        }
    }

    /// Is this operator left‑associative? (all four are)
    pub fn is_left_assoc(self) -> bool {
        true
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "x",
            Op::Div => "÷",
        };
        write!(f, "{s}")
    }
}

#[derive(PartialEq, Props, Clone)]
struct ResultProps {
    #[props(default = "0".to_string())]
    result: String,
    #[props(default = "0".to_string())]
    current_value: String,
    #[props(default = "0".to_string())]
    current_operator: String,
}

fn main() {
    dioxus::launch(App);
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
pub fn Calculator() -> Element {
    let calc_pad = [
        "7", "8", "9", "C", "AC", "4", "5", "6", "+", "-", "1", "2", "3", "x", "/", "0", ".", "00",
        "=",
    ];

    let mut result_signal = use_signal(|| "0".to_string());
    let mut current_value_signal = use_signal(|| "0".to_string());
    let mut decimal_signal = use_signal(|| false);
    let mut current_operator_signal = use_signal(|| "".to_string());

    ///TODO: Need to fix floating point calcs
    rsx! {
		div { class: "calculator",
			div { class: "text-field-container",
				DisplayField {
					result: result_signal,
					current_value: current_value_signal(),
					current_operator: current_operator_signal(),
				}
			}
			div { id: "buttons",
				for value in calc_pad {
					match value {
					    v if v.parse::<i32>().is_ok() => rsx! {
						button {
							class: "grid-item number",
							onclick: move |_| {
							    if current_value_signal() == "0" {
							        if !["0", "00"].contains(&v) {
							            if !current_operator_signal().is_empty() {
							                if current_operator_signal() == "-" {
							                    current_value_signal.set("-".to_string() + v);
							                    current_operator_signal.set("".to_string())
							                } else {
							                    current_value_signal
							                        .set("0".to_string() + &current_operator_signal() + v);
							                    current_operator_signal.set("".to_string())
							                }
							            } else {
							                tracing::debug!("Setting current value to {}", v);
							                current_value_signal.set(v.to_string())
							            }
							        }
							    } else {
							        if !current_operator_signal().is_empty() {
							            current_value_signal
							                .set(
							                    current_value_signal() + " " + &current_operator_signal() + " ",
							                );
							            current_operator_signal.set("".to_string())
							        }
							        current_value_signal.set(current_value_signal() + v)
							    }
							},
							"{value}"
						}
					},
					    "." => rsx! {
						button {
							class: "grid-item number",
							onclick: move |_| {
							    if !decimal_signal() {
							        decimal_signal.set(true);
							        if current_value_signal() == "0" {
							            current_value_signal.set("0.".to_string());
							        } else if !current_operator_signal().is_empty() {
							            current_value_signal
							                .set(current_value_signal() + &current_operator_signal() + "0.");
							            current_operator_signal.set("".to_string());
							        } else {
							            current_value_signal.set(current_value_signal() + ".");
							        }
							    }
							},
							"{value}"
						}
					},
					    "C" => rsx! {
						button {
							class: "grid-item reset",
							onclick: move |_| {
							    current_operator_signal.set("".to_string());
							    if current_value_signal() != "0" {
							        if current_value_signal().chars().count() == 1
							            || (current_value_signal().chars().count() == 2
							                && current_value_signal().starts_with('-'))
							        {
							            current_value_signal.set("0".to_string())
							        } else {
							            let mut clear_string = current_value_signal();
							            if let Some(popped_char) = clear_string.pop() {
							                if popped_char == '.' {
							                    decimal_signal.set(false);
							                }
							            }
							            if let Some(last_char) = clear_string.chars().last() {
							                if ['+', '-', 'x', '/'].contains(&last_char) {
							                    current_operator_signal.set(last_char.to_string());
							                    clear_string.pop();
							                }
							            }
							            current_value_signal.set(clear_string);
							        }
							    }
							},
							"{value}"
						}
					},
					    "AC" => rsx! {
						button {
							class: "grid-item reset",
							onclick: move |_| {
							    current_value_signal.set("0".to_string());
							    decimal_signal.set(false);
							    current_operator_signal.set("".to_string());
							},
							"{value}"
						}
					},
					    "+" | "x" | "-" | "/" => rsx! {
						button {
							class: "grid-item operator",
							onclick: move |_| {
							    current_operator_signal.set(value.to_string());
							    decimal_signal.set(false);
							},
							"{value}"
						}
					},
					    "=" => rsx! {
						button {
							class: "grid-item operator",
							onclick: move |_| {
							    current_operator_signal.set("".to_string());
							    decimal_signal.set(false);
							    let binding = current_value_signal();
							    let tokens: Vec<&str> = binding.split_whitespace().collect();
							    tracing::debug!("Tokens: {:?}", tokens);
							    tracing::debug!("Token count: {}", tokens.len());
							    if tokens.len() >= 3 {
							        let mut iter = tokens.iter();
							        let mut acc = match parse_number(iter.next().unwrap()) {
							            Ok(v) => v,
							            Err(e) => {
							                result_signal.set(e.to_string());
							                return;
							            }
							        };
							        while let (Some(op_str), Some(num_str)) = (iter.next(), iter.next()) {
							            let op = match Op::from_char(op_str.chars().next().unwrap()) {
							                Some(o) => o,
							                None => {
							                    result_signal.set(format!("invalid operator: '{op_str}'"));
							                    return;
							                }
							            };
							            let num = match parse_number(num_str) {
							                Ok(v) => v,
							                Err(e) => {
							                    result_signal.set(e.to_string());
							                    return;
							                }
							            };
							            match eval_binop(acc, op, num) {
							                Ok(v) => acc = v,
							                Err(e) => {
							                    result_signal.set(e.to_string());
							                    return;
							                }
							            }
							        }
							        result_signal.set(acc.to_string());
							    } else {
							        result_signal.set(current_value_signal().to_string());
							    }
							},
							"{value}"
						}
					},
					    _ => panic!("Wrong input"),
					}
				}
			}
		}
	}
}

#[component]
pub fn AppBar() -> Element {
    rsx! {
		nav { class: "app-bar",
			div { class: "app-bar__title", "My App" }
		}
	}
}

#[component]
pub fn DisplayField(props: ResultProps) -> Element {
    rsx! {
		{
		    if props.current_operator.is_empty() {
		        rsx! {
			div { class: "text-display", "{props.current_value}" }
		}
		    } else {
		        rsx! {
			div { class: "text-display", "{props.current_value} {props.current_operator}" }
		}
		    }
		}
		div { class: "text-display", "{props.result}" }
	}
}

fn parse_number(text: &str) -> Result<Value, CalcError> {
    if text.contains('.') {
        // Optional: parse decimals into BigRational via bigdecimal or split
        Err(CalcError::UnsupportedDecimal(text.to_string()))
    } else {
        // Arbitrary‐precision integer
        BigInt::parse_bytes(text.as_bytes(), 10)
            .map(Value::Int)
            .ok_or(CalcError::InvalidNumber(text.to_string()))
    }
}

fn eval_binop(lhs: Value, op: Op, rhs: Value) -> Result<Value, CalcError> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => match op {
            Op::Add => Ok(Value::Int(a + b)),
            Op::Sub => Ok(Value::Int(a - b)),
            Op::Mul => Ok(Value::Int(a * b)),
            Op::Div => {
                // Promote to rational for exact division:
                let rat = BigRational::new(a, b);
                Ok(Value::Rat(rat))
            }
        },
        // If either side is rational, promote both:
        (l, r) => {
            let l_rat = match l {
                Value::Int(a) => BigRational::from(a),
                Value::Rat(r) => r,
            };
            let r_rat = match r {
                Value::Int(b) => BigRational::from(b),
                Value::Rat(r) => r,
            };
            let result = match op {
                Op::Add => l_rat + r_rat,
                Op::Sub => l_rat - r_rat,
                Op::Mul => l_rat * r_rat,
                Op::Div => l_rat / r_rat,
            };
            Ok(Value::Rat(result))
        }
    }
}
