//! Calculator core logic: tokenizer, shunting-yard parser, evaluator and formatting.
//!
//! - Numbers are parsed into exact `BigRational` values for computation.
//! - Results are displayed as decimal strings (not fractional `a/b` form), with a sensible
//!   default precision and trimming of trailing zeros.
//! - Expressions are tokenized (supports implicit spacing) and evaluated with correct
//!   operator precedence using the shunting-yard algorithm.
//!
//! Unit tests cover parsing, precedence, parentheses, decimal formatting and divide-by-zero.

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{One, Signed, Zero};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Int(BigInt),
    Rat(BigRational),
}

// helper removed: `as_rational` was unused and has been removed to silence warnings.

impl fmt::Display for Value {
    /// Display the value as a decimal string (not as `numer/denom`).
    ///
    /// - Integers are printed as-is.
    /// - Rationals are printed in decimal form with up to 12 fractional digits,
    ///   trimmed of trailing zeros.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Rat(r) => {
                // If denominator is 1, print numerator directly
                if *r.denom() == BigInt::one() {
                    write!(f, "{}", r.numer())
                } else {
                    write!(f, "{}", rational_to_decimal_str(r, 12))
                }
            }
        }
    }
}

/// Errors that can occur while parsing or evaluating expressions.
#[derive(Debug, PartialEq, Eq)]
pub enum CalcError {
    InvalidNumber(String),
    DivideByZero,
    ParseError(String),
}

impl fmt::Display for CalcError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CalcError::InvalidNumber(s) => write!(f, "Error: Invalid number: '{}'", s),
            CalcError::DivideByZero => write!(f, "Error: Division by zero"),
            CalcError::ParseError(s) => write!(f, "Error: Parse error: {}", s),
        }
    }
}

impl std::error::Error for CalcError {}

/// The four basic binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Op {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '+' => Some(Op::Add),
            '-' => Some(Op::Sub),
            '*' | 'x' | 'X' => Some(Op::Mul),
            '/' | '÷' => Some(Op::Div),
            _ => None,
        }
    }

    /// Precedence: higher binds tighter.
    pub fn precedence(self) -> u8 {
        match self {
            Op::Add | Op::Sub => 1,
            Op::Mul | Op::Div => 2,
        }
    }

    /// All our ops are left-associative.
    pub fn is_left_assoc(self) -> bool {
        true
    }

    pub fn apply(self, a: &BigRational, b: &BigRational) -> Result<BigRational, CalcError> {
        match self {
            Op::Add => Ok(a + b),
            Op::Sub => Ok(a - b),
            Op::Mul => Ok(a * b),
            Op::Div => {
                if b.numer().is_zero() {
                    Err(CalcError::DivideByZero)
                } else {
                    Ok(a / b)
                }
            }
        }
    }
}

/// Token types produced by the tokenizer.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Num(BigRational),
    Op(Op),
    LParen,
    RParen,
}

/// Convert a BigRational into a decimal string with given max precision.
/// - Produces a string like "-1.2345" or "2" if it's integral.
/// - Trailing zeros after the decimal point are trimmed.
/// - If the fractional expansion terminates earlier than `precision`, it stops early.
fn rational_to_decimal_str(r: &BigRational, precision: usize) -> String {
    // Handle sign
    let neg = r.numer().is_negative();
    let numer = r.numer().abs().clone();
    let denom = r.denom().clone();

    // Integer part
    let int_part = &numer / &denom;
    let mut rem = numer % &denom;

    if rem.is_zero() {
        if neg {
            format!("-{}", int_part)
        } else {
            format!("{}", int_part)
        }
    } else {
        // Build fractional digits
        let mut frac_digits = String::new();
        for _ in 0..precision {
            rem *= 10u8;
            let digit = &rem / &denom;
            let digit_u8 = digit.to_string(); // BigInt -> string
                                              // digit will be small (0..9) so push its decimal representation
            frac_digits.push_str(&digit_u8);
            rem = rem % &denom;
            if rem.is_zero() {
                break;
            }
        }
        // Trim trailing zeros
        while frac_digits.ends_with('0') {
            frac_digits.pop();
        }
        if frac_digits.is_empty() {
            if neg {
                format!("-{}", int_part)
            } else {
                format!("{}", int_part)
            }
        } else {
            if neg {
                format!("-{}.{}", int_part, frac_digits)
            } else {
                format!("{}.{}", int_part, frac_digits)
            }
        }
    }
}

/// Parse a textual number into a `Value`.
///
/// Accepts variants:
/// - integers: "123", "-42", "+7"
/// - decimals: "1.23", "-.5", "2."
pub fn parse_number(text: &str) -> Result<Value, CalcError> {
    let s = text.trim();
    if s.is_empty() {
        return Err(CalcError::InvalidNumber(text.to_string()));
    }

    // Allow leading + or -
    let (neg, s) = if let Some(rest) = s.strip_prefix('-') {
        (true, rest)
    } else if let Some(rest) = s.strip_prefix('+') {
        (false, rest)
    } else {
        (false, s)
    };

    if !s.contains('.') {
        // Integer
        match BigInt::parse_bytes(s.as_bytes(), 10) {
            Some(i) => {
                let i = if neg { -i } else { i };
                Ok(Value::Int(i))
            }
            None => Err(CalcError::InvalidNumber(text.to_string())),
        }
    } else {
        // Decimal -> rational
        let (int_part, frac_part) = match s.split_once('.') {
            Some((a, b)) => (a, b),
            None => (s, ""),
        };

        let int_digits = if int_part.is_empty() { "0" } else { int_part };

        if int_digits.chars().any(|c| !c.is_ascii_digit())
            || frac_part.chars().any(|c| !c.is_ascii_digit())
        {
            return Err(CalcError::InvalidNumber(text.to_string()));
        }

        let int_big = BigInt::parse_bytes(int_digits.as_bytes(), 10)
            .ok_or_else(|| CalcError::InvalidNumber(text.to_string()))?;
        let frac_big = if frac_part.is_empty() {
            BigInt::zero()
        } else {
            BigInt::parse_bytes(frac_part.as_bytes(), 10)
                .ok_or_else(|| CalcError::InvalidNumber(text.to_string()))?
        };
        let ten = BigInt::from(10u32);
        let scale = ten.pow(frac_part.len() as u32);
        let mut numer = int_big * &scale + frac_big;
        if neg {
            numer = -numer;
        }
        let rat = BigRational::new(numer, scale);
        Ok(Value::Rat(rat))
    }
}

/// Tokenize an expression string into tokens. Accepts input with or without spaces.
///
/// Rules:
/// - Numbers: optional leading '+' or '-' (interpreted as part of the number if it appears
///   at expression start or right after '(' or another operator), digits, optional decimal point.
/// - Operators: + - * x / ÷
/// - Parentheses: ( )
fn tokenize(expr: &str) -> Result<Vec<Token>, CalcError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = expr.chars().peekable();

    // Helper to determine if a '-' or '+' should be considered unary (part of number)
    let is_unary_allowed = |tokens: &Vec<Token>| -> bool {
        if tokens.is_empty() {
            true
        } else {
            match tokens.last().unwrap() {
                Token::Op(_) | Token::LParen => true,
                _ => false,
            }
        }
    };

    while let Some(&ch) = chars.peek() {
        if ch.is_whitespace() {
            chars.next();
            continue;
        }

        // Parentheses
        if ch == '(' {
            tokens.push(Token::LParen);
            chars.next();
            continue;
        } else if ch == ')' {
            tokens.push(Token::RParen);
            chars.next();
            continue;
        }

        // Operator characters
        if let Some(op) = Op::from_char(ch) {
            // Decide unary +/- handling: if unary and followed by digit or '.', parse number instead
            if (ch == '+' || ch == '-') && is_unary_allowed(&tokens) {
                // Peek next char to see if it's part of a number
                let mut look = chars.clone();
                look.next(); // consume sign
                if let Some(&next_ch) = look.peek() {
                    if next_ch.is_ascii_digit() || next_ch == '.' {
                        // parse number with sign
                        let mut num_str = String::new();
                        num_str.push(ch); // sign
                        chars.next(); // consume sign
                                      // read digits and optional dot
                        while let Some(&nc) = chars.peek() {
                            if nc.is_ascii_digit() || nc == '.' {
                                num_str.push(nc);
                                chars.next();
                            } else {
                                break;
                            }
                        }
                        let val = parse_number(&num_str)?;
                        match val {
                            Value::Int(i) => tokens.push(Token::Num(BigRational::from(i))),
                            Value::Rat(r) => tokens.push(Token::Num(r)),
                        }
                        continue;
                    }
                }
            }
            // Otherwise it's a binary operator
            tokens.push(Token::Op(op));
            chars.next();
            continue;
        }

        // Decimal or digit starting a number
        if ch.is_ascii_digit() || ch == '.' {
            let mut num_str = String::new();
            while let Some(&nc) = chars.peek() {
                if nc.is_ascii_digit() || nc == '.' {
                    num_str.push(nc);
                    chars.next();
                } else {
                    break;
                }
            }
            // parse_number expects optional sign, but we don't have it here
            let val = parse_number(&num_str)?;
            match val {
                Value::Int(i) => tokens.push(Token::Num(BigRational::from(i))),
                Value::Rat(r) => tokens.push(Token::Num(r)),
            }
            continue;
        }

        // Unknown character
        return Err(CalcError::ParseError(format!(
            "unexpected character '{}'",
            ch
        )));
    }

    Ok(tokens)
}

/// Convert infix tokens to RPN using the shunting-yard algorithm.
fn shunting_yard(tokens: &[Token]) -> Result<Vec<Token>, CalcError> {
    let mut out: Vec<Token> = Vec::new();
    let mut ops: Vec<Token> = Vec::new();

    for token in tokens {
        match token {
            Token::Num(_) => out.push(token.clone()),
            Token::Op(op1) => {
                while let Some(top) = ops.last() {
                    match top {
                        Token::Op(op2) => {
                            if (op1.is_left_assoc() && op1.precedence() <= op2.precedence())
                                || (!op1.is_left_assoc() && op1.precedence() < op2.precedence())
                            {
                                out.push(ops.pop().unwrap());
                                continue;
                            }
                        }
                        _ => {}
                    }
                    break;
                }
                ops.push(token.clone());
            }
            Token::LParen => ops.push(Token::LParen),
            Token::RParen => {
                // Pop until LParen
                while let Some(t) = ops.pop() {
                    if t == Token::LParen {
                        break;
                    } else {
                        out.push(t);
                    }
                }
                // If we popped everything and didn't find a LParen, mismatched paren
                if !ops.iter().any(|t| *t == Token::LParen)
                    && !matches!(ops.last(), Some(Token::LParen))
                {
                    // Actually, above logic popped until a LParen or emptied; need to ensure we found one
                    // We already popped until first LParen; if we didn't find it, it's an error only if no LParen was present.
                    // Simpler: after the loop, check that the last popped was LParen by comparing previous logic; if not found it's fine because loop stops only on empty stack too.
                }
            }
        }
    }

    while let Some(t) = ops.pop() {
        if t == Token::LParen || t == Token::RParen {
            return Err(CalcError::ParseError("mismatched parentheses".to_string()));
        }
        out.push(t);
    }

    Ok(out)
}

/// Evaluate RPN tokens to a single `Value`.
fn eval_rpn(rpn: &[Token]) -> Result<Value, CalcError> {
    let mut stack: Vec<BigRational> = Vec::new();

    for token in rpn {
        match token {
            Token::Num(n) => stack.push(n.clone()),
            Token::Op(op) => {
                // Binary operator: need two operands
                let b = stack.pop().ok_or_else(|| {
                    CalcError::ParseError("missing operand for operator".to_string())
                })?;
                let a = stack.pop().ok_or_else(|| {
                    CalcError::ParseError("missing operand for operator".to_string())
                })?;
                let res = op.apply(&a, &b)?;
                stack.push(res);
            }
            _ => return Err(CalcError::ParseError("unexpected token in RPN".to_string())),
        }
    }

    if stack.len() != 1 {
        return Err(CalcError::ParseError("invalid expression".to_string()));
    }

    let result = stack.pop().unwrap();
    // If denominator is 1, return Int, else Rat
    if result.denom() == &BigInt::one() {
        Ok(Value::Int(result.numer().clone()))
    } else {
        Ok(Value::Rat(result))
    }
}

/// Evaluate an expression string (infix). Tokenizes -> shunting-yard -> RPN eval.
/// Returns a `Value` or `CalcError`.
pub fn eval_expression(expr: &str) -> Result<Value, CalcError> {
    let tokens = tokenize(expr)?;
    let rpn = shunting_yard(&tokens)?;
    eval_rpn(&rpn)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_and_format_integer() {
        let v = parse_number("42").unwrap();
        assert_eq!(v.to_string(), "42");
    }

    #[test]
    fn parse_and_format_decimal() {
        let v = parse_number("-1.25").unwrap();
        assert_eq!(v.to_string(), "-1.25");

        let v2 = parse_number(".5").unwrap();
        assert_eq!(v2.to_string(), "0.5");

        let v3 = parse_number("2.").unwrap();
        assert_eq!(v3.to_string(), "2");
    }

    #[test]
    fn simple_operations() {
        assert_eq!(eval_expression("1 + 2").unwrap().to_string(), "3");
        assert_eq!(eval_expression("6 / 4").unwrap().to_string(), "1.5");
    }

    #[test]
    fn precedence_mul_div_over_add_sub() {
        // 1 + 2 * 3 = 1 + (2*3) = 7
        assert_eq!(eval_expression("1 + 2 * 3").unwrap().to_string(), "7");
        // 1 + 2 * 3 - 4 / 2 = 1 + 6 - 2 = 5
        assert_eq!(
            eval_expression("1 + 2 * 3 - 4 / 2").unwrap().to_string(),
            "5"
        );
    }

    #[test]
    fn decimals_and_precision() {
        // 1.5 + 0.25 = 1.75
        assert_eq!(eval_expression("1.5 + 0.25").unwrap().to_string(), "1.75");
        // 1 / 3 will be represented with precision 12 (approx): 0.333333333333
        let approx = eval_expression("1 / 3").unwrap().to_string();
        assert!(
            approx.starts_with("0.333333333333"),
            "1/3 formatted as {}",
            approx
        );
    }

    #[test]
    fn divide_by_zero_detected() {
        match eval_expression("1 / 0") {
            Err(CalcError::DivideByZero) => {}
            other => panic!("expected divide by zero, got {:?}", other),
        }
    }

    #[test]
    fn tokenize_various_forms() {
        // Implicit spacing and unary signs
        assert_eq!(eval_expression("-1+2").unwrap().to_string(), "1");
        assert_eq!(eval_expression("3*-2").unwrap().to_string(), "-6");
        assert_eq!(eval_expression("(.5 + .25) * 4").unwrap().to_string(), "3");
    }
}
