use rand::Rng;
use std::collections::HashMap;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub};

use syntax::{Adverb, Formula};

#[derive(Debug)]
pub enum Error {
    Symbol,
    // TODO: split the generic errors into variants
    Generic(&'static str),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Error::Generic(str) => write!(f, "{}", str),
            Error::Symbol => write!(f, "symbol not set"),
        }
    }
}

pub struct Interpretation {
    bindings: HashMap<String, Formula>,
}

impl Interpretation {
    pub fn new() -> Interpretation {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn reduce(&mut self, tree: Formula) -> Result<Formula, Error> {
        match tree {
            Formula::Number(_) => Ok(tree),
            Formula::Rop(op, r) => self.reduce_rop(&op, *r),
            Formula::Arop(adv, op, r) => self.reduce_arop(adv, &op, *r),
            Formula::Lrop(l, op, r) => self.reduce_lrop(*l, &op, *r),
            Formula::Array(mut array) => self.reduce_array(&mut array),
            Formula::Table(mut shape, mut data) => self.reduce_table(&mut shape, &mut data),
            Formula::Symbol(sym) => self.bindings.get(&sym).cloned().ok_or(Error::Symbol),
        }
    }

    fn reduce_rop(&mut self, op: &str, f: Formula) -> Result<Formula, Error> {
        let f = self.reduce(f)?;
        match op {
            "+" => Ok(f),
            "-" => {
                match f {
                    Formula::Number(n) => Ok(Formula::Number(-n)),

                    Formula::Array(array) => {
                        let mut acc = vec![];
                        for v in array {
                            match v {
                                Formula::Number(n) => acc.push(Formula::Number(-n)),
                                _ => return Err(Error::Generic("unexpected condition")),
                            }
                        }
                        Ok(Formula::Array(acc))
                    }

                    Formula::Table(shape, data) => {
                        let mut acc = vec![];
                        for v in data {
                            match v {
                                Formula::Number(n) => acc.push(Formula::Number(-n)),
                                _ => return Err(Error::Generic("unexpected condition")),
                            }
                        }
                        Ok(Formula::Table(shape, acc))
                    }

                    // The reduce above should have turned f into atomic data.
                    _ => Err(Error::Generic("unexpected condition")),
                }
            }

            "deal" => match f {
                Formula::Number(n) => {
                    let mut rng = rand::thread_rng();
                    Ok(Formula::Number(rng.gen_range(0..n)))
                }

                Formula::Array(vec) => {
                    let mut rng = rand::thread_rng();
                    let mut res: Vec<Formula> = Vec::new();
                    for v in vec {
                        let val = self.reduce(v)?;
                        if let Formula::Number(n) = val {
                            if n < 0 {
                                res.push(Formula::Number(rng.gen_range(n..0)));
                            } else {
                                res.push(Formula::Number(rng.gen_range(0..n)));
                            }
                        } else {
                            return Err(Error::Generic("`deal` is only implemented for numbers"));
                        }
                    }
                    Ok(Formula::Array(res))
                }

                Formula::Table(shape, data) => {
                    let mut acc = vec![];
                    for x in &data {
                        let y = Box::new(x.clone());
                        acc.push(self.reduce(Formula::Rop("deal".to_string(), y))?);
                    }
                    Ok(Formula::Table(shape, acc))
                }
                _ => Err(Error::Generic("`deal` is only implemented for numbers")),
            },

            "shape" => Ok(match f {
                Formula::Number(_) => Formula::Number(0),
                Formula::Array(v) => Formula::Number(v.len() as i64),
                Formula::Table(shape, _) => Formula::Array(shape),

                _ => return Err(Error::Generic("shape can't op on this")),
            }),

            "enum" => match f {
                Formula::Number(n) => Ok(Formula::Array((0..n).map(Formula::Number).collect())),
                _ => Err(Error::Generic("enum can only op on numbers")),
            },

            "rot" => match f {
                Formula::Array(mut vec) => {
                    let mut acc = vec![];

                    while let Some(f) = vec.pop() {
                        acc.push(self.reduce(f)?);
                    }

                    Ok(Formula::Array(acc))
                }

                Formula::Table(shape, data) => Ok(if shape.len() == 2 {
                    Formula::Table(vec![shape[1].clone(), shape[0].clone()], data)
                } else {
                    Formula::Table(shape, data)
                }),

                _ => Ok(f),
            },

            _ => Err(Error::Generic("monad unimplemented")),
        }
    }

    fn reduce_lrop(&mut self, l: Formula, op: &str, r: Formula) -> Result<Formula, Error> {
        let rhs = self.reduce(r)?; // reduce right first; defer lhs in case op is an assign
        Ok(match op {
            "+" | "-" | "/" | "*" | "%" | "|" | "&" | "^" => {
                let lhs = self.reduce(l)?;
                self.reduce_basic_lrop(lhs, op, rhs)?
            }

            "=" => {
                if let Formula::Symbol(id) = l {
                    self.bindings.insert(id, rhs.clone());
                    rhs
                } else {
                    return Err(Error::Generic("lhs is not a symbol"));
                }
            }

            "shape" => {
                let seed = match self.reduce(rhs)? {
                    Formula::Number(n) => vec![Formula::Number(n)],
                    Formula::Array(a) => a,
                    _ => return Err(Error::Generic("shape: rhs must be Array or number")),
                };

                let shape = match self.reduce(l)? {
                    Formula::Array(array) => array,
                    Formula::Number(n) => vec![Formula::Number(n)],
                    _ => return Err(Error::Generic("error: (num)+ shape (num)+")),
                };

                let mut data = Vec::new();
                let gen = seed.iter().cycle();
                let elements =
                    self.reduce_arop(Adverb::Over, "*", Formula::Array(shape.clone()))?;
                if let Formula::Number(n) = elements {
                    for x in gen.take(n as usize) {
                        data.push(x.clone());
                    }
                    if shape.len() == 1 {
                        Formula::Array(data)
                    } else {
                        Formula::Table(shape, data)
                    }
                } else {
                    return Err(Error::Generic("couldn't reduce"));
                }
            }

            "drop" => {
                let lhs = self.reduce(l)?;
                if let (Formula::Number(mut n), Formula::Array(data)) = (lhs, rhs) {
                    let mut a = vec![];
                    if n > 0 {
                        if n > data.len() as i64 {
                            return Err(Error::Generic("not enough elements to drop"));
                        }
                        for x in data {
                            if n > 0 {
                                n -= 1;
                                continue;
                            }
                            a.push(self.reduce(x)?);
                        }
                    } else {
                        n += data.len() as i64;
                        if n < 0 {
                            return Err(Error::Generic("n drop x y z..."));
                        }
                        for x in data {
                            if n == 0 {
                                break;
                            }
                            n -= 1;
                            a.push(self.reduce(x)?);
                        }
                    }
                    Formula::Array(a)
                } else {
                    return Err(Error::Generic("n drop x y z..."));
                }
            }

            "take" => {
                let lhs = self.reduce(l)?;
                if let (Formula::Number(mut n), Formula::Array(data)) = (lhs, rhs) {
                    if n == 0 {
                        return Err(Error::Generic("n take x y z... with n>0"));
                    }

                    let mut a = vec![];
                    if n > 0 {
                        let mut extensions = if n - data.len() as i64 > 0 {
                            n - data.len() as i64
                        } else {
                            0
                        };

                        for x in data {
                            if n == 0 {
                                break;
                            }
                            a.push(self.reduce(x)?);
                            n -= 1;
                        }
                        while extensions > 0 {
                            a.push(Formula::Number(0));
                            extensions -= 1;
                        }
                    } else {
                        n *= -1;
                        let mut skips = (data.len() as i64) - n;

                        for x in data {
                            if skips > 0 {
                                skips -= 1;
                                continue;
                            }
                            a.push(self.reduce(x)?);
                        }
                    }

                    Formula::Array(a)
                } else {
                    return Err(Error::Generic("n take x y z..."));
                }
            }
            _ => return Err(Error::Generic("unimplemented op")),
        })
    }

    fn reduce_vec(&mut self, v: &mut Vec<Formula>) -> Result<Vec<Formula>, Error> {
        let mut acc = Vec::new();
        while let Some(x) = v.pop() {
            acc.push(self.reduce(x)?);
        }
        acc.reverse();
        Ok(acc)
    }

    fn reduce_array(&mut self, array: &mut Vec<Formula>) -> Result<Formula, Error> {
        let acc = self.reduce_vec(array)?;
        Ok(Formula::Array(acc))
    }

    fn reduce_table(
        &mut self,
        shape: &mut Vec<Formula>,
        data: &mut Vec<Formula>,
    ) -> Result<Formula, Error> {
        let data = self.reduce_vec(data)?;
        let shape = self.reduce_vec(shape)?;
        Ok(Formula::Table(shape, data))
    }

    fn reduce_arop(&mut self, adv: Adverb, lrop: &str, r: Formula) -> Result<Formula, Error> {
        match adv {
            Adverb::Over => {
                let r = self.reduce(r)?;
                let mut v = match r {
                    Formula::Array(v) => v,
                    Formula::Number(_) => return Ok(r),
                    // TODO: Formula::Table => op along rows (or something)
                    _ => return Err(Error::Generic("expected array or num")),
                };

                let mut acc = match v.pop() {
                    Some(f) => self.reduce(f)?,
                    _ => return Err(Error::Generic("expected non-zero length array")),
                };
                while let Some(val) = v.pop() {
                    acc = self.reduce_lrop(val, lrop, acc)?;
                }
                Ok(acc)
            }

            // Scan uses k's semantics.
            //
            // # gyre
            // -\enum 5
            // 0 -1 -3 -6 -10
            //
            // # apl
            // -\0 1 2 3 4
            // 0 -1 1 -2 2
            Adverb::Scan => {
                let r = self.reduce(r)?;
                let v = match r {
                    Formula::Array(v) => v,
                    Formula::Number(_) => return Ok(r),
                    _ => return Err(Error::Generic("expected array or num")),
                };

                let mut iter = v.into_iter();
                let mut acc = match iter.next() {
                    Some(f) => self.reduce(f)?,
                    _ => return Err(Error::Generic("expected non-zero length array")),
                };
                let mut result = vec![acc.clone()];

                for val in iter {
                    acc = self.reduce_lrop(acc, lrop, val)?;
                    result.push(acc.clone());
                }
                Ok(Formula::Array(result))
            }
        }
    }

    fn reduce_basic_lrop(&mut self, l: Formula, op: &str, r: Formula) -> Result<Formula, Error> {
        match (l, r) {
            (Formula::Number(a), Formula::Number(b)) => {
                Ok(Formula::Number(Self::lookup_gadget(op)?(a, b)))
            }

            (Formula::Number(n), Formula::Array(a)) => {
                let mut res = Vec::new();

                for x in a {
                    if let Formula::Number(k) = x {
                        res.push(Formula::Number(Self::lookup_gadget(op)?(n, k)));
                    } else {
                        return Err(Error::Generic("not a number?"));
                    }
                }
                Ok(Formula::Array(res))
            }

            (Formula::Number(n), Formula::Table(shape, data)) => {
                let mut acc = Vec::new();
                for x in data.into_iter() {
                    acc.push(self.reduce_lrop(Formula::Number(n), op, x)?);
                }
                Ok(self.reduce(Formula::Table(shape, acc))?)
            }

            (Formula::Array(x), Formula::Array(y)) => {
                if x.len() != y.len() {
                    return Err(Error::Generic("len mismatch"));
                }

                let mut acc = Vec::new();

                for (a, b) in x.into_iter().zip(y.into_iter()) {
                    if let (Formula::Number(a), Formula::Number(b)) = (a, b) {
                        acc.push(Formula::Number(Self::lookup_gadget(op)?(a, b)));
                    } else {
                        return Err(Error::Generic("probably won't get here"));
                    }
                }
                Ok(Formula::Array(acc))
            }

            (Formula::Array(a), Formula::Number(n)) => {
                let mut res = Vec::new();

                for x in a {
                    if let Formula::Number(k) = x {
                        res.push(Formula::Number(Self::lookup_gadget(op)?(k, n)));
                    } else {
                        return Err(Error::Generic("not a number"));
                    }
                }
                Ok(Formula::Array(res))
            }

            (Formula::Table(shape, a), Formula::Table(_, b)) => {
                let mut res = Vec::new();

                for (x, y) in a.into_iter().zip(b.into_iter()) {
                    if let (Formula::Number(x), Formula::Number(y)) = (x, y) {
                        res.push(Formula::Number(Self::lookup_gadget(op)?(x, y)));
                    } else {
                        return Err(Error::Generic("rust loves dead branches"));
                    }
                }

                Ok(Formula::Table(shape, res))
            }

            _ => Err(Error::Generic("basic lrop unimplemented")),
        }
    }

    fn lookup_gadget(op: &str) -> Result<fn(i64, i64) -> i64, Error> {
        match op.into() {
            Gadget::Add => Ok(i64::add),
            Gadget::Sub => Ok(i64::sub),
            Gadget::Mul => Ok(i64::mul),
            Gadget::Div => Ok(safe_div), // hacky
            Gadget::Rem => Ok(i64::rem),
            Gadget::BitOr => Ok(i64::bitor),
            Gadget::BitAnd => Ok(i64::bitand),
            Gadget::BitXor => Ok(i64::bitxor),
            Gadget::Unsupported => Err(Error::Generic("op is not supported")),
        }
    }
}

fn safe_div(a: i64, b: i64) -> i64 {
    if b == 0 {
        return 0;
    };
    i64::div(a, b)
}

enum Gadget {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitOr,
    BitAnd,
    BitXor,
    Unsupported,
}

impl From<&str> for Gadget {
    fn from(op: &str) -> Gadget {
        match op {
            "+" => Gadget::Add,
            "-" => Gadget::Sub,
            "*" => Gadget::Mul,
            "/" => Gadget::Div,
            "%" => Gadget::Rem,
            "|" => Gadget::BitOr,
            "&" => Gadget::BitAnd,
            "^" => Gadget::BitXor,
            _ => Gadget::Unsupported,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_number() {
        let mut terp = Interpretation::new();
        let f = Formula::Number(1);
        assert_eq!(terp.reduce(f).unwrap(), Formula::Number(1));
    }

    #[test]
    fn test_list() {
        let mut terp = Interpretation::new();
        let f = Formula::Array(vec![Formula::Number(1)]);
        assert_eq!(terp.reduce(f.clone()).unwrap(), f);
    }

    #[test]
    fn test_rop() {
        let mut terp = Interpretation::new();
        let f = Formula::Rop("enum".to_string(), Box::new(Formula::Number(5)));
        assert_eq!(
            terp.reduce(f).unwrap(),
            Formula::Array(
                vec![0, 1, 2, 3, 4]
                    .into_iter()
                    .map(Formula::Number)
                    .collect()
            )
        );
    }

    #[test]
    fn test_num_lrop_num() {
        let mut terp = Interpretation::new();
        let a = Formula::Number(1);
        let f = Formula::Lrop(Box::new(a.clone()), "+".to_string(), Box::new(a));
        let y = Formula::Number(2);
        assert_eq!(terp.reduce(f).unwrap(), y);
    }

    #[test]
    fn test_num_lrop_array() {
        let mut terp = Interpretation::new();
        let a = Formula::Number(1);
        let f = Formula::Lrop(Box::new(a.clone()), "+".to_string(), Box::new(a));
        let y = Formula::Number(2);
        assert_eq!(terp.reduce(f).unwrap(), y);
    }

    #[test]
    fn test_array_lrop_array() {
        let mut terp = Interpretation::new();
        let f = Formula::Lrop(
            Box::new(Formula::Array(vec![Formula::Number(1), Formula::Number(2)])),
            "+".to_string(),
            Box::new(Formula::Array(vec![Formula::Number(3), Formula::Number(4)])),
        );
        let y = Formula::Array(vec![Formula::Number(4), Formula::Number(6)]);
        assert_eq!(terp.reduce(f).unwrap(), y);
    }
}
