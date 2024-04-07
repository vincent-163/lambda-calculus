// Chapter 1: Untyped lambda calculus
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Lambda(Box<Term>),
    App(Box<Term>, Box<Term>),
    DeBruijn(u64),
}

impl std::fmt::Display for Term {
    fn fmt(&self, writer: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.print(writer, 0)
    }
}

impl Term {
    pub fn print<W: Write>(&self, writer: &mut W, mut index: u64) -> Result<(), std::fmt::Error> {
        match self {
            Term::Lambda(term) => {
                write!(writer, "(λ{}. ", index_to_letter(index))?;
                index += 1;
                term.print(writer, index)?;
                write!(writer, ")")
            }
            Term::App(term1, term2) => {
                write!(writer, "(")?;
                term1.print(writer, index)?;
                write!(writer, " ")?;
                term2.print(writer, index)?;
                write!(writer, ")")
            }
            Term::DeBruijn(i) => {
                let de_bruijn_index = index - 1 - i;
                let letter = index_to_letter(de_bruijn_index);
                write!(writer, "{}", letter)
            }
        }
    }

    pub fn beta_reduction(self) -> Term {
        match self {
            Term::Lambda(x) => Term::Lambda(Box::new(x.beta_reduction())),
            Term::DeBruijn(x) => Term::DeBruijn(x),
            Term::App(x, y) => match *x {
                Term::Lambda(x) => Self::apply(*x, &y, 0),
                Term::DeBruijn(i) => Term::App(Box::new(Term::DeBruijn(i)), Box::new(y.beta_reduction())),
                _ => Term::App(Box::new(x.beta_reduction()), y),
            }
        }
    }

    pub fn beta_reduction_inner(self) -> (Term, bool) {
        match self {
            Term::Lambda(x) => { let (y, b) = x.beta_reduction_inner(); (Term::Lambda(Box::new(y)), b) },
            Term::DeBruijn(x) => (Term::DeBruijn(x), false),
            Term::App(x, y) => match *x {
                Term::Lambda(x) => (Self::apply(*x, &y, 0), true),
                Term::DeBruijn(i) => { let (y, b) = y.beta_reduction_inner(); (Term::App(Box::new(Term::DeBruijn(i)), Box::new(y)), b) },
                _ => { let (x, b) = x.beta_reduction_inner(); (Term::App(Box::new(x), y), b) } ,
            }
        }
    }

    pub fn beta_reduction_full(self) -> Term {
        let mut s = self;
        let mut b = true;
        while b {
            (s, b) = Self::beta_reduction_inner(s);
        }
        s
    }

    pub fn bump(x: Term, n: u64, k: u64) -> Term {
        match x {
            Term::Lambda(xx) => Term::Lambda(Box::new(Self::bump(*xx, n, k+1))),
            Term::App(a, b) => Term::App(Box::new(Self::bump(*a, n, k)), Box::new(Self::bump(*b, n, k))),
            Term::DeBruijn(i) => Term::DeBruijn(if i >= k { i+n } else { i }),
        }
    }

    pub fn apply(x: Term, y: &Term, index: u64) -> Term {
        match x {
            Term::Lambda(xx) => Term::Lambda(Box::new(Self::apply(*xx, y, index+1))),
            Term::App(a, b) => Term::App(Box::new(Self::apply(*a, y, index)), Box::new(Self::apply(*b, y, index))),
            Term::DeBruijn(i) => {
                match i.cmp(&index) {
                    std::cmp::Ordering::Less => Term::DeBruijn(i),
                    std::cmp::Ordering::Equal => Self::bump(y.clone(), index, 0),
                    std::cmp::Ordering::Greater => Term::DeBruijn(i - 1),
                }
            }
        }
    }
}

fn index_to_letter(mut index: u64) -> String {
    let mut letter = String::new();
    loop {
        let char_code = (index % 26) as u8 + b'a';
        letter.insert(0, char_code as char);
        index /= 26;
        if index == 0 {
            break;
        }
        index -= 1;
    }
    letter
}
