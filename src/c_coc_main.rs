mod b_coc;
mod c_coc;
use c_coc::{Token, TermParser, next_token};

const proof: &'static str = "λ#λa;λ∀b;c;λ∀c;∀d;#λ∀d;%%b;a;a;λ∀e;∀f;∀g;∀%%e;c;b;∀%%f;c;b;%%g;e;c;λ∀f;∀g;∀%%e;%f;b;%f;a;%%f;c;b;λ∀g;∀#∀%%f;%g;b;h;b;λ∀∀h;#∀%a;h;∀∀j;%c;%i;a;∀k;%d;a;λ∀i;∀j;k;λ∀j;%%h;%%b;j;a;a;λ∀k;∀l;%%j;%%d;%k;b;a;%k;%%d;b;a;λ∀l;∀m;n;λ∀m;%%k;%%b;m;a;a;λ∀n;∀o;%%m;%%d;%n;b;a;%%g;a;%d;%b;a;%e;%m;n;";
fn main() {
    let mut parser = TermParser::new();
    let mut iter = proof.chars();
    println!("{}", proof);
    while let Some(token) = next_token(&mut iter) {
        println!("{:?}", token);
        let res = parser.next(token);
        if let Some(res) = res {
            println!("{:?}", res);
            break;
        }
        println!("{:?}", parser);
    }
    println!("{:?}", parser);
}