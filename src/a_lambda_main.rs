// Calculus of constructions

// Chapter 1: Untyped lambda calculus
mod a_lambda;

use a_lambda::Term;
use std::io;

fn l(x: Term) -> Term {
    Term::Lambda(Box::new(x))
}

fn a(x: Term, y: Term) -> Term {
    Term::App(Box::new(x), Box::new(y))
}

fn b(x: u64) -> Term {
    Term::DeBruijn(x)
}

fn n(x: u64) -> Term {
    let mut y = b(0);
    for i in 0..x {
        y = a(b(1), y);
    }
    l(l(y))
}

fn main() {
    let id = l(b(0));
    let two = l(l(a(b(1), a(b(1), b(0)))));
    println!("{}", two);
    let t = l(l(b(1)));
    println!("{}", t);
    println!("{}", t.beta_reduction());
    let t = l(b(0));
    println!("{}", t);
    println!("{}", t.beta_reduction());
    let y = a(id.clone(), two.clone());
    println!("{}", y);
    println!("{}", y.beta_reduction());
    let mut y = a(two.clone(), two.clone());
    println!("{}", y);
    for i in 0..15 {
        y = y.beta_reduction();
        println!("{}", y);
    }
    println!("{}", n(3));
    println!("{}", a(n(3),n(3)).beta_reduction_full());
    println!("{}", n(27));
    println!("{}", a(n(3),n(3)).beta_reduction_full() == n(27));
    println!("{}", a(n(3),n(3)).beta_reduction_full() == n(26));

    // \a.\b.\f.\x.(a f) ((b f) x)
    let add = l(l(l(l(
        a(a(b(3), b(1)), a(a(b(2), b(1)), b(0)))
    ))));
    let mut y = a(a(add,n(3)),n(5));
    println!("{}", y.clone().beta_reduction_full());

    // pred: λn.λf.λx. n (λg.λh. h (g f)) (λu.x) (λu.u)
    let pred = l(l(l(
        a(
            a(
                a(b(2), l(l(a(b(0), a(b(1), b(3)))))), l(b(1))
            ), l(b(0)))
    )));
    let mut y = a(pred.clone(), n(5));
    println!("{}", y.beta_reduction_full());
    println!("{}", a(n(2),pred.clone()).beta_reduction_full());
    println!("{}", a(a(n(2),pred.clone()),n(5)).beta_reduction_full());
}
