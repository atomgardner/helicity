use std::str::FromStr;

use scanner;
use scanner::Scanner;
use scanner::Token;
use syntax::{Adverb, Formula};

#[derive(Debug, PartialEq)]
pub enum Error<'a> {
    UnexpectedToken(Token<'a>), // TODO: these need more context
    TokenStream,
    Scanner(scanner::Error),
    Number,
    Unreachable,
}

impl From<scanner::Error> for Error<'_> {
    fn from(err: scanner::Error) -> Self {
        Error::Scanner(err)
    }
}

impl std::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Error::UnexpectedToken(tok) => write!(f, "unexpected token: {}", tok),
            Error::TokenStream => write!(f, "token stream ended prematurely"),
            Error::Unreachable => write!(f, "unreachable branch: can't find builtin lrop"),
            Error::Scanner(err) => write!(f, "scanner error: {}", err),
            Error::Number => write!(f, "only i64 are supported"),
        }
    }
}

type Result<'a, T> = std::result::Result<T, Error<'a>>;

pub struct Parser<'input> {
    scanner: Scanner<'input>,
    cache: Option<Token<'input>>,
}

impl<'input> Parser<'input> {
    pub fn new(r: &'input str) -> Parser {
        Parser {
            scanner: Scanner::new(r),
            cache: None,
        }
    }

    pub fn next(&mut self) -> Result<'input, Option<Token<'input>>> {
        if self.cache.is_some() {
            Ok(self.cache.take())
        } else {
            match self.scanner.next() {
                None => Ok(None),
                Some(res) => match res {
                    Ok(tok) => Ok(Some(tok)),
                    Err(err) => Err(Error::Scanner(err)),
                },
            }
        }
    }

    fn peek(&mut self) -> Result<'input, Option<Token<'input>>> {
        if self.cache.is_some() {
            Ok(self.cache)
        } else {
            match self.scanner.next() {
                Some(result) => Ok(result.map(|tok| {
                    self.cache.replace(tok);
                    self.cache
                })?),
                None => Ok(None),
            }
        }
    }

    /// expect tests whether `t` is next in the token stream, and, if so, bumps the scanner; if not,
    /// throws an error.
    fn expect(&mut self, t: Token) -> Result<'input, ()> {
        if let Some(s) = self.peek()? {
            if s == t {
                self.next()?;
                Ok(())
            } else {
                Err(Error::UnexpectedToken(s))
            }
        } else {
            Err(Error::TokenStream)
        }
    }

    /// Parse assembles the token stream into a syntax tree for the following grammar.
    ///
    /// formula :=
    ///   | rop formula
    ///   | stuff
    ///   | stuff lrop formula
    ///
    /// stuff :=
    ///   | thing
    ///   | thing stuff
    ///
    /// thing :=
    ///   | number
    ///   | symbol
    ///   | "(" formula ")"
    ///
    /// number := "0".."9"+
    /// rop    := "+" | "-" | "enum" | ...
    /// lrop   := "+" | "-" | "join" | ...
    pub fn parse<'a>(&'a mut self) -> Result<'input, Formula> {
        let tree = self.parse_formula();
        match self.next()? {
            Some(Token::LineFeed) | None => tree,
            Some(t) => Err(Error::UnexpectedToken(t)),
        }
    }

    fn parse_formula(&mut self) -> Result<'input, Formula> {
        match self.peek()? {
            Some(Token::Verb(mon)) => {
                self.next()?;
                if mon.find(|c: char| matches!(c, '/' | '\\')).is_some() {
                    self.parse_arop(mon)
                } else {
                    Ok(Formula::Rop(
                        mon.to_string(),
                        Box::new(self.parse_formula()?),
                    ))
                }
            }
            Some(Token::Symbol(sym)) => {
                if builtin::rop(sym) {
                    self.next()?;
                    return Ok(Formula::Rop(
                        sym.to_string(),
                        Box::new(self.parse_formula()?),
                    ));
                }
                let thing = self.parse_stuff()?;
                self.parse_lrop(thing)
            }
            Some(Token::Number(_)) | Some(Token::LeftParens) => {
                let thing = self.parse_stuff()?;
                self.parse_lrop(thing)
            }
            Some(tok) => Err(Error::UnexpectedToken(tok)),
            None => Err(Error::TokenStream),
        }
    }

    fn parse_arop(&mut self, lrop: &str) -> Result<'input, Formula> {
        if let Some(i) = lrop.find('/') {
            return Ok(Formula::Arop(
                Adverb::Over,
                lrop[..i].to_string(),
                Box::new(self.parse_formula()?),
            ));
        } else if let Some(i) = lrop.find('\\') {
            Ok(Formula::Arop(
                Adverb::Scan,
                lrop[..i].to_string(),
                Box::new(self.parse_formula()?),
            ))
        } else {
            Err(Error::Unreachable)
        }
    }

    fn parse_stuff(&mut self) -> Result<'input, Formula> {
        let thing = self.parse_thing()?;
        let mut stuff = vec![thing];

        loop {
            match self.peek()? {
                Some(Token::LeftParens) | Some(Token::Number(_)) => stuff.push(self.parse_thing()?),
                Some(Token::Symbol(sym)) => {
                    if builtin::lrop(sym) {
                        break;
                    }
                    stuff.push(self.parse_thing()?);
                }
                _ => break,
            }
        }

        Ok(if stuff.len() == 1 {
            stuff.pop().unwrap()
        } else {
            Formula::Array(stuff)
        })
    }

    fn parse_thing(&mut self) -> Result<'input, Formula> {
        Ok(match self.next()? {
            Some(Token::Number(num)) => {
                if let Ok(n) = i64::from_str(num) {
                    Formula::Number(n)
                } else {
                    return Err(Error::Number);
                }
            }
            Some(Token::Symbol(sym)) => Formula::Symbol(sym.to_string()),
            Some(Token::LeftParens) => {
                let line = self.parse_formula()?;
                self.expect(Token::RightParens)?;
                line
            }
            Some(tok) => return Err(Error::UnexpectedToken(tok)),
            None => return Err(Error::TokenStream),
        })
    }

    fn parse_lrop(&mut self, stuff: Formula) -> Result<'input, Formula> {
        let tok = self.peek()?;
        match tok {
            Some(Token::Verb(op)) => {
                self.next()?;
                Ok(Formula::Lrop(
                    Box::new(stuff),
                    op.to_string(),
                    Box::new(self.parse_formula()?),
                ))
            }
            Some(Token::Symbol(op)) => {
                // TODO: tokenize builtins into Token::Verb
                if builtin::lrop(op) {
                    self.next()?;
                    Ok(Formula::Lrop(
                        Box::new(stuff),
                        op.to_string(),
                        Box::new(self.parse_formula()?),
                    ))
                } else {
                    Err(Error::UnexpectedToken(Token::Symbol(op)))
                }
            }
            Some(Token::RightParens) | Some(Token::LineFeed) | None => Ok(stuff),
            Some(tok) => Err(Error::UnexpectedToken(tok)),
        }
    }
}

mod builtin {
    pub fn rop(s: &str) -> bool {
        matches!(s, "enum" | "shape" | "deal" | "rot")
    }

    pub fn lrop(s: &str) -> bool {
        matches!(s, "xor" | "and" | "or" | "shape" | "join" | "drop" | "take")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenstream() {
        let mut p = Parser::new("(1 + 1)");
        assert_eq!(Ok(Some(Token::LeftParens)), p.next());
        assert_eq!(Ok(Some(Token::Number("1"))), p.next());
        assert_eq!(Ok(Some(Token::Verb("+"))), p.next());
        assert_eq!(Ok(Some(Token::Number("1"))), p.next());
        assert_eq!(Ok(Some(Token::RightParens)), p.next());
        assert_eq!(Ok(Some(Token::LineFeed)), p.next());
        assert_eq!(Ok(None), p.next());
        assert_eq!(Ok(None), p.next());
        assert_eq!(Ok(None), p.next());
        assert_eq!(Ok(None), p.next());
    }

    #[test]
    fn lookahead() {
        let mut p = Parser::new("(1 + 1)");
        assert_eq!(Ok(Some(Token::LeftParens)), p.peek());
        assert_eq!(Ok(Some(Token::LeftParens)), p.next());

        assert_eq!(Ok(Some(Token::Number("1"))), p.peek());
        assert_eq!(Ok(Some(Token::Number("1"))), p.next());

        assert_eq!(Ok(Some(Token::Verb("+"))), p.peek());
        assert_eq!(Ok(Some(Token::Verb("+"))), p.next());

        assert_eq!(Ok(Some(Token::Number("1"))), p.peek());
        assert_eq!(Ok(Some(Token::Number("1"))), p.next());

        assert_eq!(Ok(Some(Token::RightParens)), p.peek());
        assert_eq!(Ok(Some(Token::RightParens)), p.next());

        assert_eq!(Ok(Some(Token::LineFeed)), p.peek());
        assert_eq!(Ok(Some(Token::LineFeed)), p.next());

        assert_eq!(Ok(None), p.peek());
        assert_eq!(Ok(None), p.next());
    }

    #[test]
    fn parenthetical() {
        let mut p = Parser::new("(1 + 1)");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Lrop(
                Box::new(Formula::Number(1)),
                "+".to_string(),
                Box::new(Formula::Number(1))
            )
        );
    }

    #[test]
    fn list() {
        let mut p = Parser::new("1 1");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Array(vec![Formula::Number(1), Formula::Number(1)])
        );
    }

    #[test]
    fn formula_in_list() {
        let mut p = Parser::new("(1 + 1) 1 1");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Array(vec![
                Formula::Lrop(
                    Box::new(Formula::Number(1)),
                    "+".to_string(),
                    Box::new(Formula::Number(1))
                ),
                Formula::Number(1),
                Formula::Number(1),
            ])
        );
    }

    #[test]
    fn formula_in_middle_of_list() {
        let mut p = Parser::new("2 (1 + 1) 1 1");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Array(vec![
                Formula::Number(2),
                Formula::Lrop(
                    Box::new(Formula::Number(1)),
                    "+".to_string(),
                    Box::new(Formula::Number(1))
                ),
                Formula::Number(1),
                Formula::Number(1),
            ])
        );
    }

    #[test]
    fn formula_at_end_of_list() {
        let mut p = Parser::new("1 2 3 (2+2)");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Array(vec![
                Formula::Number(1),
                Formula::Number(2),
                Formula::Number(3),
                Formula::Lrop(
                    Box::new(Formula::Number(2)),
                    "+".to_string(),
                    Box::new(Formula::Number(2))
                ),
            ])
        );
    }

    #[test]
    fn dyad_with_formula_in_list() {
        let mut p = Parser::new("1 + (1 + 1) 1 1");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Lrop(
                Box::new(Formula::Number(1)),
                "+".to_string(),
                Box::new(Formula::Array(vec![
                    Formula::Lrop(
                        Box::new(Formula::Number(1)),
                        "+".to_string(),
                        Box::new(Formula::Number(1))
                    ),
                    Formula::Number(1),
                    Formula::Number(1),
                ])),
            )
        );
    }

    #[test]
    fn unbalanced_parens() {
        let mut p = Parser::new("(1+1))");
        assert_eq!(Ok(Some(Token::LeftParens)), p.next());
        assert_eq!(Ok(Some(Token::Number("1"))), p.next());
        assert_eq!(Ok(Some(Token::Verb("+"))), p.next());
        assert_eq!(Ok(Some(Token::Number("1"))), p.next());
        assert_eq!(Ok(Some(Token::RightParens)), p.next());
        assert_eq!(Ok(Some(Token::RightParens)), p.next());
    }

    #[test]
    fn basic_parse() {
        let mut p = Parser::new("1 + 1");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Lrop(
                Box::new(Formula::Number(1)),
                "+".to_string(),
                Box::new(Formula::Number(1))
            )
        );
    }

    #[test]
    fn number_lrop_list() {
        let mut p = Parser::new("1 + 1 1 1");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Lrop(
                Box::new(Formula::Number(1)),
                "+".to_string(),
                Box::new(Formula::Array(vec![
                    Formula::Number(1),
                    Formula::Number(1),
                    Formula::Number(1),
                ]))
            )
        );
    }

    #[test]
    fn list_lrop_list() {
        let mut p = Parser::new("1 1 1 + 1 1 1");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Lrop(
                Box::new(Formula::Array(vec![
                    Formula::Number(1),
                    Formula::Number(1),
                    Formula::Number(1),
                ])),
                "+".to_string(),
                Box::new(Formula::Array(vec![
                    Formula::Number(1),
                    Formula::Number(1),
                    Formula::Number(1),
                ]))
            )
        );
    }

    #[test]
    fn unreduced_list_lrop_list() {
        let mut p = Parser::new("(1+1) 1 1 + 1 1 1");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Lrop(
                Box::new(Formula::Array(vec![
                    Formula::Lrop(
                        Box::new(Formula::Number(1)),
                        "+".to_string(),
                        Box::new(Formula::Number(1)),
                    ),
                    Formula::Number(1),
                    Formula::Number(1),
                ])),
                "+".to_string(),
                Box::new(Formula::Array(vec![
                    Formula::Number(1),
                    Formula::Number(1),
                    Formula::Number(1),
                ]))
            )
        );
    }

    #[test]
    fn list_lrop_unreduced_list() {
        let mut p = Parser::new("1 1 1 + (1+1) 1 1");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Lrop(
                Box::new(Formula::Array(vec![
                    Formula::Number(1),
                    Formula::Number(1),
                    Formula::Number(1),
                ])),
                "+".to_string(),
                Box::new(Formula::Array(vec![
                    Formula::Lrop(
                        Box::new(Formula::Number(1)),
                        "+".to_string(),
                        Box::new(Formula::Number(1)),
                    ),
                    Formula::Number(1),
                    Formula::Number(1),
                ])),
            )
        );
    }

    #[test]
    fn unreducedlist_lrop_unreduced_list() {
        let mut p = Parser::new("1 (1+1) 1 + (1+1) 1 1");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Lrop(
                Box::new(Formula::Array(vec![
                    Formula::Number(1),
                    Formula::Lrop(
                        Box::new(Formula::Number(1)),
                        "+".to_string(),
                        Box::new(Formula::Number(1)),
                    ),
                    Formula::Number(1),
                ])),
                "+".to_string(),
                Box::new(Formula::Array(vec![
                    Formula::Lrop(
                        Box::new(Formula::Number(1)),
                        "+".to_string(),
                        Box::new(Formula::Number(1)),
                    ),
                    Formula::Number(1),
                    Formula::Number(1),
                ])),
            )
        );
    }

    #[test]
    fn trailing_parens() {
        let mut p = Parser::new("1 + 2(");
        assert_eq!(
            p.parse().unwrap_err(),
            Error::UnexpectedToken(Token::LineFeed),
        );
        let mut p = Parser::new("1 + 2)");
        assert_eq!(
            p.parse().unwrap_err(),
            Error::UnexpectedToken(Token::RightParens),
        );
    }

    #[test]
    fn nested_parens() {
        let mut p = Parser::new("((1 + 2) 1 2)");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Array(vec![
                Formula::Lrop(
                    Box::new(Formula::Number(1)),
                    "+".to_string(),
                    Box::new(Formula::Number(2)),
                ),
                Formula::Number(1),
                Formula::Number(2),
            ]),
        );
    }

    #[test]
    fn nested_lists() {
        let mut p = Parser::new("((1 2) 3 4) 5 6");
        assert_eq!(
            p.parse().unwrap(),
            Formula::Array(vec![
                Formula::Array(vec![
                    Formula::Array(vec![Formula::Number(1), Formula::Number(2),]),
                    Formula::Number(3),
                    Formula::Number(4),
                ]),
                Formula::Number(5),
                Formula::Number(6),
            ])
        );
    }
}
