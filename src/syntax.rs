#[derive(Debug, Clone, PartialEq)]
pub enum Formula {
    Number(i64),
    Symbol(String),
    Array(Vec<Formula>),
    Table(Vec<Formula>, Vec<Formula>), // shape, data
    Rop(String, Box<Formula>),
    Arop(Adverb, String, Box<Formula>),
    Lrop(Box<Formula>, String, Box<Formula>),
}

impl Formula {
    pub fn expect_number(&self) -> i64 {
        match self {
            Formula::Number(n) => *n,
            _ => panic!("expected number"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Adverb {
    Over, // fold
    Scan,
}

impl std::fmt::Display for Adverb {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Adverb::Over => write!(f, "over"),
            Adverb::Scan => write!(f, "scan"),
        }
    }
}

fn fmt_table(
    f: &mut std::fmt::Formatter<'_>,
    prefix: &str,
    width: usize,
    shape: &[Formula],
    data: &[Formula],
) -> std::result::Result<(), std::fmt::Error> {
    match shape.len() {
        0 => write!(f, ""),
        1 => write!(f, ""),
        2 => {
            let rows = shape[0].expect_number() as usize;
            let cols = shape[1].expect_number() as usize;

            writeln!(f, "{}", prefix)?;
            for (k, x) in data.iter().take(cols * rows).enumerate() {
                if k > 0 {
                    if k % cols == 0 {
                        writeln!(f)?;
                    } else {
                        write!(f, " ")?;
                    }
                }
                if let Formula::Number(n) = x {
                    write!(f, "{:width$}", n)?
                }
            }
            writeln!(f)
        }

        _ => {
            // expensive
            let stride = {
                let mut stride = 1_usize;
                for s in shape[1..].iter() {
                    stride *= s.expect_number() as usize;
                }
                stride
            };

            let n = if let Some(Formula::Number(n)) = shape.iter().next() {
                *n as usize
            } else {
                panic!("expected number")
            };

            for k in 0..n {
                let prefix = if shape[1..].len() == 2 {
                    format!("{} {} * *]:", prefix, k)
                } else {
                    format!("{} {}", prefix, k)
                };

                fmt_table(f, &prefix, width, &shape[1..], &data[k * stride..])?;
            }

            Ok(())
        }
    }
}

impl std::fmt::Display for Formula {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Formula::Number(n) => write!(f, "{}", n),

            Formula::Array(xs) => {
                for (k, x) in xs.iter().enumerate() {
                    if k > 0 {
                        write!(f, " ")?
                    }
                    write!(f, "{}", x)?
                }
                Ok(())
            }

            Formula::Table(shape, data) => {
                match shape.len() {
                    1 => {
                        for x in data {
                            write!(f, "{} ", x)?
                        }
                    }

                    _ => {
                        let mut width = 1_usize;
                        for x in data {
                            let t = format!("{}", x).len();
                            if width < t {
                                width = t
                            }
                        }
                        let prefix = if shape.len() == 2 { "" } else { "[" };
                        fmt_table(f, prefix, width, &shape, &data)?
                    }
                }
                Ok(())
            }

            Formula::Symbol(symbol) => write!(f, "{}", symbol),
            Formula::Rop(op, x) => write!(f, "({} {})", op, x),
            Formula::Arop(a, op, x) => write!(f, "(({}{}) {})", op, a, x),
            Formula::Lrop(l, op, r) => write!(f, "(({}) <{}> ({}))", l, op, r),
        }
    }
}
