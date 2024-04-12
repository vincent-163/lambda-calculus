use b_coc::{TermTree, Mode};
use std::sync::Arc;

mod b_coc;

// fn T() -> Term { Term::Set }
// fn P() -> Term { Term::Prop }
// fn l(a: Term, b: Term) -> Term { Term::Lam(Box::new(a), Box::new(b)) }
// fn f(a: Term, b: Term) -> Term { Term::Pi(Box::new(a), Box::new(b)) }
// fn a(a: Term, b: Term) -> Term { Term::App(Box::new(a), Box::new(b)) }
// fn b(a: usize) -> Term { Term::Var(a) }

const proof: &'static str = "λ#λa;λ∀b;c;λ∀c;∀d;#λ∀d;%%b;a;a;λ∀e;∀f;∀g;∀%%e;c;b;∀%%f;c;b;%%g;e;c;λ∀f;∀g;∀%%e;%f;b;%f;a;%%f;c;b;λ∀g;∀#∀%%f;%g;b;h;b;λ∀∀h;#∀%a;h;∀∀j;%c;%i;a;∀k;%d;a;λ∀i;∀j;k;λ∀j;%%h;%%b;j;a;a;λ∀k;∀l;%%j;%%d;%k;b;a;%k;%%d;b;a;λ∀l;∀m;n;λ∀m;%%k;%%b;m;a;a;λ∀n;∀o;%%m;%%d;%n;b;a;%%g;a;%d;%b;a;%e;%m;n;";
fn test(x: &str) {
    println!("{}", x);
    let tt = Arc::new(TermTree::parse(x));
    println!("{:?}", tt);
    let val = tt.eval(Mode::Value, &Arc::new(b_coc::Context::Tail));
    // println!("val: {:?}", val);
    let val = val.quote(0);
    println!("val: {:?}", val);
    println!("val compact: {}", val.compact());
    let typ = tt.eval(Mode::Type, &Arc::new(b_coc::Context::Tail)).quote(0);
    println!("typ: {:?}", typ);
    println!("typ compact: {}", typ.compact());
}
fn main() {
    // Test cases verified via js:
    // x:* y:* z:* a:(.x y) b:(.(.x y) z) (b a)
    // test("λ#λ#λ#λ∀c;c;λ∀∀d;d;c;%a;b;"); // ∀#∀#∀#∀∀c;c;∀∀∀d;d;c;c;
    
    // x:* y:* z:* a:(.x y) b:(.y z) c:x (b (a c))
    // test("λ#λ#λ#λ∀c;c;λ∀c;c;λe;%b;%c;a;"); // ∀#∀#∀#∀∀c;c;∀∀c;c;∀e;d;

    // x:* y:(t.x t) z:x (y z)
    // test("λ#λ∀a;a;λb;%b;a;"); // ∀#∀∀a;a;∀b;a;
    // WRONG

    // let tt = TermTree::parse(proof);
    // println!("{:?}", tt);
    // test("λ#a;");
    // test("λ#λ#λ∀a;b;a;"); // ∀#∀#∀∀a;b;∀a;b;
    // test("λ#λ#λ∀a;b;%a;b;"); // Type mismatch for APP: fun typ ID(1), arg typ SET
    // test("λ#λ#λa;λ∀b;d;%a;b;"); // ∀#∀#∀a;∀∀b;c;c;
    // test("∀#∀#∀a;∀∀b;c;c;"); // #
    // test("λ#λ#λa;λ∀b;c;%a;c;"); // Type mismatch for APP: fun typ ID(1), arg typ SET
    // test("λ#λ#λa;λ∀b;c;λ#%b;c;"); // ∀#∀#∀a;∀∀b;c;∀#b;
    // test("∀#∀#∀a;∀∀b;c;∀#b;"); // #
    // test("∀#∀#∀#∀∀c;b;∀∀∀d;c;b;b;"); // #
    // %%l;%%f;n;%m;n;%m;n;
    // test("∀#∀a;#");
    // test(proof);
    // correct
    // test("∀#∀a;∀∀b;c;∀∀c;∀d;#∀∀d;%%b;a;a;∀∀e;∀f;∀g;∀%%e;c;b;∀%%f;c;b;%%g;e;c;∀∀f;∀g;∀%%e;%f;b;%f;a;%%f;c;b;∀∀g;∀#∀%%f;%g;b;h;b;∀∀∀h;#∀%a;h;∀∀j;%c;%i;a;∀k;%d;a;∀∀i;∀j;k;∀∀j;%%h;%%b;j;a;a;∀∀k;∀l;%%j;%%d;%k;b;a;%k;%%d;b;a;∀∀l;∀m;n;∀∀m;%%k;%%b;m;a;a;∀∀n;∀o;%%m;%%d;%n;b;a;%%g;a;%d;%b;a;%%l;%%f;n;%m;n;%m;n;");
    // 
    // println!("{:?}", P().type_check(&vec![]));
    // println!("{:?}", T().type_check(&vec![]));
    // println!("{:?}", l(P(), b(0)).type_check(&vec![]));
    // println!("{:?}", l(P(), l(P(), b(0))).type_check(&vec![]));
    // println!("{:?}", l(P(), l(P(), b(1))).type_check(&vec![]));
    // println!("{:?}", l(P(), l(P(), b(2))).type_check(&vec![]));

    // test("λ#λ∀a;b;%%");

    // https://github.com/VictorTaelin/calculus-of-constructions/issues/2
    test("λ∀##λ∀#b;b;");
    test("∀∀##∀∀#b;∀##");
}