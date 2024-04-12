mod d_coc;
use std::sync::Arc;
use d_coc::{Token, PartialTerm, next_token, TermTree};

const proof: &'static str = "λ#λa;λ∀b;c;λ∀c;∀d;#λ∀d;%%b;a;a;λ∀e;∀f;∀g;∀%%e;c;b;∀%%f;c;b;%%g;e;c;λ∀f;∀g;∀%%e;%f;b;%f;a;%%f;c;b;λ∀g;∀#∀%%f;%g;b;h;b;λ∀∀h;#∀%a;h;∀∀j;%c;%i;a;∀k;%d;a;λ∀i;∀j;k;λ∀j;%%h;%%b;j;a;a;λ∀k;∀l;%%j;%%d;%k;b;a;%k;%%d;b;a;λ∀l;∀m;n;λ∀m;%%k;%%b;m;a;a;λ∀n;∀o;%%m;%%d;%n;b;a;%%g;a;%%d;b;a;%%d;n;%m;n;";
fn test(expr: &str) -> Result<(Arc<TermTree>, Arc<TermTree>), ()> {
    let mut parser = PartialTerm::empty();
    let mut iter = expr.chars();
    println!("{}", expr);
    while let Some(token) = next_token(&mut iter) {
        println!("{:?}", token);
        let prev_parser = parser.clone();
        let res = parser.next(token);
        if let Ok(Some((typ, bod))) = res {
            // println!("Last parser: {:?}", prev_parser);
            println!("Last token: {:?}", token);
            let mut s = String::new();
            typ.iter_tokens().for_each(|tok| tok.append_to(&mut s));
            println!("typ: {}", s);
            let mut s = String::new();
            bod.iter_tokens().for_each(|tok| tok.append_to(&mut s));
            println!("bod: {}", s);
            println!("org: {}", expr);
            return Ok((typ, bod));
        } else if let Err(res) = res {
            println!("err: {:?}", res);
            break;
        }
        println!("{:?}", parser);
    }
    Err(())
    // println!("{:?}", parser);
}

fn main() {
    // test("λ∀##λ∀#b;b;"); // right side of for not set
    // test("∀∀##∀∀#b;∀##");
    // test("λ#λa;a;");
    // test("λ#λa;λ∀b;c;λ∀#∀a;∀b;#%a;d;");
    // test("λ##");
    let (typ, bod) = test(proof).unwrap();
    assert_eq!(typ.to_string(), "∀#∀a;∀∀b;c;∀∀c;∀d;#∀∀d;%%b;a;a;∀∀e;∀f;∀g;∀%%e;c;b;∀%%f;c;b;%%g;e;c;∀∀f;∀g;∀%%e;%f;b;%f;a;%%f;c;b;∀∀g;∀#∀%%f;%g;b;h;b;∀∀∀h;#∀%a;h;∀∀j;%c;%i;a;∀k;%d;a;∀∀i;∀j;k;∀∀j;%%h;%%b;j;a;a;∀∀k;∀l;%%j;%%d;%k;b;a;%k;%%d;b;a;∀∀l;∀m;n;∀∀m;%%k;%%b;m;a;a;∀∀n;∀o;%%m;%%d;%n;b;a;%%g;a;%%d;b;a;%%l;%%f;%m;n;%m;n;%m;%%f;n;%m;n;");
}