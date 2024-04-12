use std::{ops::Deref, sync::Arc};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token {
    SET,
    VAR(usize),
    LAM,
    FOR,
    APP,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TermTree {
    SET(),
    VAR(usize), // DeBruijn index
    LAM(Arc<TermTree>, Arc<TermTree>),
    FOR(Arc<TermTree>, Arc<TermTree>),
    APP(Arc<TermTree>, Arc<TermTree>),
    ERR(String),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TermTreeLazyBody {
    pub typ: Arc<TermTreeLazy>,
    pub bod: Arc<TermTree>,
    pub mode: Mode, 
    pub ctx: Arc<Context>
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TermTreeLazy {
    SET(),
    VAR(usize), // DeBruijn index
    LAM(TermTreeLazyBody),
    FOR(TermTreeLazyBody),
    APP(Arc<TermTreeLazy>, Arc<TermTreeLazy>),
    ERR(String),
}

impl std::fmt::Debug for TermTreeLazy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SET() => f.debug_tuple("SET").finish(),
            Self::VAR(arg0) => f.debug_tuple("VAR").field(arg0).finish(),
            Self::LAM(arg0) => f.debug_tuple("LAM").field(arg0).finish(),
            Self::FOR(arg0) => f.debug_tuple("FOR").field(arg0).finish(),
            Self::APP(arg0, arg1) => f.debug_tuple("APP").field(arg0).field(arg1).finish(),
            Self::ERR(arg0) => write!(f, "ERR: {}", arg0),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Context {
    Tail,
    Head { typ: Arc<TermTreeLazy>, bod: Arc<TermTreeLazy>, next: Arc<Context>, id: usize },
}

impl std::fmt::Debug for TermTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SET() => f.debug_tuple("SET").finish(),
            Self::VAR(arg0) => f.debug_tuple("VAR").field(arg0).finish(),
            Self::LAM(arg0, arg1) => f.debug_tuple("LAM").field(arg0).field(arg1).finish(),
            Self::FOR(arg0, arg1) => f.debug_tuple("FOR").field(arg0).field(arg1).finish(),
            Self::APP(arg0, arg1) => f.debug_tuple("APP").field(arg0).field(arg1).finish(),
            Self::ERR(arg0) => write!(f, "ERR: {}", arg0),
        }
    }
}

impl std::fmt::Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Self::Tail => write!(f, "Tail"),
            // Self::Head { typ, bod, next, id } => f.debug_struct("Head").field("typ", typ).field("bod", bod).field("next", next).field("id", id).finish(),
            Self::Tail => writeln!(f, "Tail"),
            Self::Head { typ, bod, next, id } => {
                writeln!(f, "{:>4} {:?} {:?}", id-1, typ.quote(*id-1), bod.quote(*id))?; std::fmt::Debug::fmt(next, f)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Mode {
    Type,
    Value,
}

impl Context {
    pub fn get(&self, i: usize, mode: Mode) -> Option<Arc<TermTreeLazy>> {
        match self {
            Context::Tail => None,
            Context::Head { typ, bod, next, .. } => if i == 0 {
                match mode {
                    Mode::Type => Some(typ.clone()),
                    Mode::Value => Some(bod.clone()),
                }
            } else {
                (next.get(i-1, mode))
            }
        }
    }

    pub fn getboth(&self, i: usize) -> Option<(Arc<TermTreeLazy>, Arc<TermTreeLazy>)> {
        match self {
            Context::Tail => None,
            Context::Head { typ, bod, next, .. } => if i == 0 {
                Some((typ.clone(), bod.clone()))
            } else {
                next.getboth(i-1)
            }
        }
    }

    pub fn id(&self) -> usize {
        match self {
            Context::Tail => 0,
            Context::Head { id, .. } => *id,
        }
    }

    pub fn extend(self: &Arc<Self>, mut typ: Arc<TermTreeLazy>, bod: Arc<TermTreeLazy>) -> Arc<Context> {
        Arc::new(Context::Head { typ, bod, next: self.clone(), id: self.id()+1 })
    }
}

impl TermTreeLazyBody {
    pub fn apply(&self, term: Arc<TermTreeLazy>) -> Arc<TermTreeLazy> {
        self.bod.eval(self.mode, &self.ctx.extend(self.typ.clone(), term))
    }
}

impl TermTreeLazy {
    pub fn quote(&self, d: usize) -> Arc<TermTree> {
        match self {
            Self::SET() => Arc::new(TermTree::SET()),
            Self::VAR(n) => Arc::new(TermTree::VAR(d-n-1)),
            Self::LAM(body) => Arc::new(TermTree::LAM(body.typ.quote(d), body.apply(Arc::new(TermTreeLazy::VAR(d))).quote(d+1))),
            Self::FOR(body) => Arc::new(TermTree::FOR(body.typ.quote(d), body.apply(Arc::new(TermTreeLazy::VAR(d))).quote(d+1))),
            Self::APP(a, b) => Arc::new(TermTree::APP(a.quote(d), b.quote(d))),
            Self::ERR(e) => Arc::new(TermTree::ERR(e.to_string())),
        }
    }
}

pub fn check(a: &Arc<TermTreeLazy>, b: &Arc<TermTreeLazy>, d: usize) -> bool {
    match (Deref::deref(a), Deref::deref(b)) {
        (TermTreeLazy::SET(), TermTreeLazy::SET()) => true,
        (TermTreeLazy::VAR(x), TermTreeLazy::VAR(y)) => x == y,
        (TermTreeLazy::LAM(a), TermTreeLazy::LAM(b)) =>
            check(&a.typ, &b.typ, d) &&
            check(&a.apply(Arc::new(TermTreeLazy::VAR(d))), &b.apply(Arc::new(TermTreeLazy::VAR(d))), d+1),
        (TermTreeLazy::FOR(a), TermTreeLazy::FOR(b)) =>
            check(&a.typ, &b.typ, d) &&
            check(&a.apply(Arc::new(TermTreeLazy::VAR(d))), &b.apply(Arc::new(TermTreeLazy::VAR(d))), d+1),
        (TermTreeLazy::APP(aa, ab), TermTreeLazy::APP(ba, bb)) =>
            check(aa, ba, d) && check(ab, bb, d),
        _ => false,
    }
}

fn var_name(mut x: usize, s: &mut String)  {
    loop {
        s.push(('a' as u8 + (x%26) as u8) as char);
        x /= 26;
        if x == 0 { break }
    }
}

impl TermTree {
    pub fn compact(&self) -> String {
        let mut s = String::new();
        self.compact_inner(&mut s, 0);
        s
    }
    fn compact_inner(&self, s: &mut String, d: usize) {
        match self {
            TermTree::SET() => s.push('#'),
            TermTree::VAR(x) => { var_name(*x, s); s.push(';') },
            TermTree::LAM(typ, bod) => { s.push('λ'); typ.compact_inner(s, d); bod.compact_inner(s, d+1) },
            TermTree::FOR(typ, bod) => { s.push('∀'); typ.compact_inner(s, d); bod.compact_inner(s, d+1) },
            TermTree::APP(fun, arg) => { s.push('%'); fun.compact_inner(s, d); arg.compact_inner(s, d) },
            TermTree::ERR(_) => s.push('#'),
        }
    }
    pub fn parse(s: &str) -> Self {
        let mut x = s.chars();
        Self::parse_chars(&mut x)
    }
    fn parse_chars(s: &mut std::str::Chars<'_>) -> Self {
        match s.next() {
            None => TermTree::ERR("Premature termination".to_owned()),
            Some('#') => TermTree::SET(),
            Some('λ') => {
                let typ = Arc::new(Self::parse_chars(s));
                let bod = Arc::new(Self::parse_chars(s));
                TermTree::LAM(typ, bod)
            },
            Some('∀') => {
                let typ = Arc::new(Self::parse_chars(s));
                let bod = Arc::new(Self::parse_chars(s));
                TermTree::FOR(typ, bod)
            },
            Some('%') => {
                let fun = Arc::new(Self::parse_chars(s));
                let arg = Arc::new(Self::parse_chars(s));
                TermTree::APP(fun, arg)
            }
            Some(x) if x >= 'a' && x <= 'z' => {
                let mut ind: usize = (x as usize) - ('a' as usize);
                let mut b: usize = 1;
                loop {
                    if let Some(x) = s.next() {
                        if x >= 'a' && x <= 'z' {
                            b = b * 26;
                            ind += b * ((x as usize) - ('a' as usize));
                        } else if x == ';' {
                            break TermTree::VAR(ind)
                        } else {
                            break TermTree::ERR("Invalid DeBruijn index notation, does not end at ;".to_owned())
                        }
                    } else {
                        break TermTree::ERR("Premature termination".to_owned())
                    }
                }
            },
            _ => TermTree::ERR("unrecognized token".to_owned()),
        }
    }
    pub fn eval(self: &Arc<Self>, mode: Mode, ctx: &Arc<Context>) -> Arc<TermTreeLazy> {
        match Deref::deref(self) {
            TermTree::SET() => match mode {
                // Mode::Type => TermTree::ERR("SET does not have a type".to_owned()),
                Mode::Type => Arc::new(TermTreeLazy::SET()),
                Mode::Value => Arc::new(TermTreeLazy::SET()),
            },
            TermTree::VAR(n) => {
                match ctx.get(*n, mode) {
                    Some(x) => x.clone(),
                    None => Arc::new(TermTreeLazy::ERR("DeBruijn index out of range".to_owned()))
                }
            },
            TermTree::APP(fun, arg) => {
                match mode {
                    Mode::Type => {
                        let funt = fun.eval(Mode::Type, ctx);
                        if let TermTreeLazy::FOR(funt) = Deref::deref(&funt) {
                            let argt = arg.eval(Mode::Type, ctx);
                            let argv = arg.eval(Mode::Value, ctx);
                            if check(&funt.typ, &argt, 0) {
                                funt.apply(argv)
                            } else {
                                println!("Type mismatch for APP: fun {:?}, arg {:?}", fun, arg);
                                println!("{:?}", ctx);
                                println!("funt.typ: {:?}", funt.typ.quote(ctx.id()));
                                println!("argt: {:?}", argt.quote(ctx.id()));
                                Arc::new(TermTreeLazy::ERR(format!("Type mismatch for APP: fun {:?}, arg {:?}", fun, arg)))
                            }
                        } else {
                            Arc::new(TermTreeLazy::ERR(format!("Left side of APP not a function: fun {:?}, arg {:?}", fun, arg)))
                        }
                    },
                    Mode::Value => {
                        let funv = fun.eval(Mode::Value, ctx);
                        if let TermTreeLazy::LAM(funv) = Deref::deref(&funv) {
                            let argt = arg.eval(Mode::Type, ctx);
                            let argv = arg.eval(Mode::Value, ctx);
                            if check(&funv.typ, &argt, 0) {
                                funv.apply(argv)
                            } else {
                                Arc::new(TermTreeLazy::ERR(format!("Type mismatch for APP: fun {:?}, arg {:?}", fun, arg)))
                            }
                            // bod.eval(Mode::Value, &ctx.extend(*typ, argv))
                        } else {
                            let argv = arg.eval(Mode::Value, ctx);
                            Arc::new(TermTreeLazy::APP(funv, argv))
                        }
                    }
                }
            },
            TermTree::FOR(typ, bod) => {
                match mode {
                    Mode::Type => {
                        let typt = typ.eval(Mode::Type, ctx);
                        let typv = typ.eval(Mode::Value, ctx);
                        let bodt = bod.eval(Mode::Type, &ctx.extend(Arc::new(TermTreeLazy::SET()), typv));
                        if !check(&typt, &Arc::new(TermTreeLazy::SET()), 0) {
                            Arc::new(TermTreeLazy::ERR(format!("In FOR, type is not of type Set: {:?}", typ)))
                        } else if !check(&bodt, &Arc::new(TermTreeLazy::SET()), 0) {
                            Arc::new(TermTreeLazy::ERR(format!("In FOR, body is not of type Set when given Set: {:?}", bod)))
                        } else {
                            Arc::new(TermTreeLazy::SET())
                        }
                    },
                    Mode::Value => {
                        let typv = typ.eval(Mode::Value, ctx);
                        Arc::new(TermTreeLazy::FOR(TermTreeLazyBody { typ: typv, bod: bod.clone(), mode: Mode::Value, ctx: ctx.clone() }))
                    }
                }
            },
            TermTree::LAM(typ, bod) => {
                let typv = typ.eval(Mode::Value, ctx);
                match mode {
                    Mode::Type => Arc::new(TermTreeLazy::FOR(TermTreeLazyBody { typ: typv, bod: bod.clone(), mode: Mode::Type, ctx: ctx.clone() })),
                    Mode::Value => Arc::new(TermTreeLazy::LAM(TermTreeLazyBody { typ: typv, bod: bod.clone(), mode: Mode::Value, ctx: ctx.clone() })),
                }
            },
            TermTree::ERR(x) => Arc::new(TermTreeLazy::ERR(x.to_string())),
        }
    }

    pub fn iter_tokens(&self) -> impl Iterator<Item = Token> + '_ {
        TermTreeIter(vec![self])
    }
}

struct TermTreeIter<'a>(Vec<&'a TermTree>);
impl Iterator for TermTreeIter<'_> {
    type Item = Token;
    
    fn next(&mut self) -> Option<Self::Item> {
        match self.0.pop() {
            Some(TermTree::SET()) => Some(Token::SET),
            Some(TermTree::VAR(x)) => Some(Token::VAR(*x)),
            Some(TermTree::LAM(x, y)) => { self.0.extend([Deref::deref(y), x].iter()); Some(Token::LAM) },
            Some(TermTree::FOR(x, y)) => { self.0.extend([Deref::deref(y), x].iter()); Some(Token::FOR) },
            Some(TermTree::APP(x, y)) => { self.0.extend([Deref::deref(y), x].iter()); Some(Token::APP) },
            Some(TermTree::ERR(_)) => None,
            None => None,
        }
    }
}