#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Token<'a> {
    Number(&'a str),
    Verb(&'a str),
    Symbol(&'a str),
    LeftParens,
    RightParens,
    LineFeed,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Number(str) => write!(f, "<num: {}>", str),
            Self::Verb(str) => write!(f, "<verb: {}>", str),
            Self::Symbol(str) => write!(f, "<symbol: {}>", str),
            Self::LeftParens => write!(f, "<left parens>"),
            Self::RightParens => write!(f, "<right parens>"),
            Self::LineFeed => write!(f, "<lf>"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    UnexpectedRune(char),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::UnexpectedRune(c) => write!(f, "scan failed: unexpected rune: {}", c),
        }
    }
}

enum Action<'a> {
    Continue,
    Yield(Option<Token<'a>>),
    Err(Error),
}

// Scanner converts src into Tokens.
//
// The Scanner sees src as a sequence of UTF-8 codepoints. Internal access to the sequence is
// managed with the now() and soon() heads. The heads are shift()ed to move through the sequence.
pub struct Scanner<'src> {
    // source string being scanned
    src: &'src str,
    // offset into source that points to the start of the next token
    tail: usize,
    // offset into source that increments from tail until a token is issued
    head: usize,
    // state machine
    state: fn(&mut Scanner<'src>) -> Action<'src>,
}

impl<'src> Scanner<'src> {
    pub fn new(r: &'src str) -> Scanner<'src> {
        Scanner {
            src: r,
            head: 0,
            tail: 0,
            state: Self::scan_next,
        }
    }

    fn now(&self) -> char {
        self.src[self.head..].chars().next().unwrap_or('\n')
    }

    fn soon(&mut self) -> Option<char> {
        if self.head >= self.src.len() {
            None
        } else {
            let width = self.now().len_utf8();
            self.src[self.head + width..].chars().next()
        }
    }

    fn shift(&mut self) -> bool {
        let more_runes = self.head < self.src.len();
        if more_runes {
            self.head += self.now().len_utf8();
        }
        more_runes
    }

    fn scan(&mut self) -> Result<Option<Token<'src>>, Error> {
        loop {
            return match (self.state)(self) {
                Action::Yield(t) => Ok(t),
                Action::Continue => continue,
                Action::Err(err) => Err(err),
            };
        }
    }

    fn scan_done(&mut self) -> Action<'src> {
        Action::Yield(None)
    }

    fn scan_comment(&mut self) -> Action<'src> {
        while self.now() != '\n' {
            self.shift();
        }
        self.state = Self::scan_next;
        Action::Yield(None)
    }

    fn scan_next(s: &mut Scanner<'src>) -> Action<'src> {
        s.tail = s.head;
        match s.now() {
            '0'..='9' => {
                s.state = Self::scan_number;
                Action::Continue
            }

            'a'..='z' | 'A'..='Z' => {
                s.state = Self::scan_symbol;
                Action::Continue
            }

            '#' => {
                // never read the comments
                s.state = Self::scan_comment;
                Action::Continue
            }

            ' ' | '\t' => {
                s.shift();
                Action::Continue
            }

            '*' | '/' | '%' | '=' | '|' | '&' | '^' => {
                if let Some('/') | Some('\\') = s.soon() {
                    s.shift(); // adverbial
                }
                s.shift();
                return Action::Yield(Some(Token::Verb(&s.src[s.tail..s.head])));
            }

            '+' | '-' => {
                match s.soon() {
                    Some('/') | Some('\\') => {
                        s.shift();
                    } // adverbial
                    Some(n) => {
                        if n.is_numeric() {
                            s.state = Self::scan_number;
                            return Action::Continue;
                        }
                    }
                    _ => (),
                };

                s.shift();
                Action::Yield(Some(Token::Verb(&s.src[s.tail..s.head])))
            }

            '\n' => {
                s.shift();
                s.state = Self::scan_done;
                Action::Yield(Some(Token::LineFeed))
            }

            '(' => {
                s.shift();
                Action::Yield(Some(Token::LeftParens))
            }

            ')' => {
                s.shift();
                Action::Yield(Some(Token::RightParens))
            }
            _ => Action::Err(Error::UnexpectedRune(s.now())),
        }
    }

    /// scan anything that starts with +|-|0..9
    fn scan_number(s: &mut Scanner<'src>) -> Action<'src> {
        let tail: usize = s.head;

        while s.shift() {
            let c = s.now();
            if !c.is_numeric() {
                // Force `1+2` to scan as Num(1) Verb(+) Num(2)
                if matches!(c, '+' | '-' | '*' | '/' | '%') {
                    s.state = Self::scan_sigil;
                } else {
                    s.state = Self::scan_next;
                }
                break;
            }
        }

        return Action::Yield(Some(Token::Number(&s.src[tail..s.head])));
    }

    fn scan_sigil(&mut self) -> Action<'src> {
        let tail = self.head;
        self.shift();
        self.state = Self::scan_next;
        return Action::Yield(Some(Token::Verb(&self.src[tail..self.head])));
    }

    /// scans anything that starts with an alpha
    fn scan_symbol(s: &mut Scanner<'src>) -> Action<'src> {
        while s.shift() {
            let c = s.now();
            if c.is_whitespace() || !c.is_alphanumeric() {
                if matches!(c, '/' | '\\') {
                    s.shift();
                }
                s.state = Self::scan_next;
                break;
            }
        }
        return Action::Yield(Some(Token::Symbol(&s.src[s.tail..s.head])));
    }
}

impl<'input> Iterator for Scanner<'input> {
    type Item = Result<Token<'input>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.scan().transpose();
        tok
    }
}

#[cfg(test)]
mod tests {
    use super::{Scanner, Token};

    fn run_test(input: &str, tokens: Vec<Token>) {
        let mut s = Scanner::new(input);
        for want in tokens.into_iter() {
            assert_eq!(Some(Ok(want)), Iterator::next(&mut s));
        }
        assert_eq!(Iterator::next(&mut s), None);
    }

    #[test]
    fn test_number() {
        let input = "-1123456\n";
        let expect = [Token::Number("-1123456"), Token::LineFeed];
        let s = Scanner::new(input);

        for (want, &got) in s.into_iter().zip(expect.iter()) {
            assert_eq!(want, Ok(got));
        }
    }

    #[test]
    fn test_numbers() {
        let input = "-1 1 2 3 4 5 6\n";
        let tokens = vec![
            Token::Number("-1"),
            Token::Number("1"),
            Token::Number("2"),
            Token::Number("3"),
            Token::Number("4"),
            Token::Number("5"),
            Token::Number("6"),
            Token::LineFeed,
        ];
        run_test(input, tokens);
    }

    #[test]
    fn test_basic_scan() {
        let input = "1 + enum 5\n";
        let expect = vec![
            Token::Number("1"),
            Token::Verb("+"),
            Token::Symbol("enum"),
            Token::Number("5"),
            Token::LineFeed,
        ];
        run_test(input, expect);
    }

    #[test]
    fn test_parens_scan() {
        let input = "(foo bar 1 2 3)\n";
        let tokens = vec![
            Token::LeftParens,
            Token::Symbol("foo"),
            Token::Symbol("bar"),
            Token::Number("1"),
            Token::Number("2"),
            Token::Number("3"),
            Token::RightParens,
            Token::LineFeed,
        ];
        run_test(input, tokens);
    }

    #[test]
    fn test_no_whitespace() {
        let input = "1+2";
        let tokens = vec![
            Token::Number("1"),
            Token::Verb("+"),
            Token::Number("2"),
            Token::LineFeed,
        ];
        run_test(input, tokens);
    }

    #[test]
    fn test_whitespace() {
        let input = "1 +2";
        let tokens = vec![Token::Number("1"), Token::Number("+2"), Token::LineFeed];
        run_test(input, tokens);
    }

    #[test]
    fn test_negative_whitespace() {
        let input = "1 -2";
        let tokens = vec![Token::Number("1"), Token::Number("-2"), Token::LineFeed];
        run_test(input, tokens);
    }

    #[test]
    fn test_funky_whitespace() {
        let input = "1 + 2+ 3 +4";
        let tokens = vec![
            Token::Number("1"),
            Token::Verb("+"),
            Token::Number("2"),
            Token::Verb("+"),
            Token::Number("3"),
            Token::Number("+4"),
            Token::LineFeed,
        ];
        run_test(input, tokens);
    }

    #[test]
    fn test_average() {
        let input = "+/enum 100\n";
        let tokens = vec![
            Token::Verb("+/"),
            Token::Symbol("enum"),
            Token::Number("100"),
            Token::LineFeed,
        ];
        run_test(input, tokens);
    }
}
