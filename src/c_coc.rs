use std::{ops::Deref, sync::Arc};

use crate::b_coc::{Mode};

// in Token, variables are stored as DeBruijn index
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token {
    SET,
    VAR(usize),
    LAM,
    FOR,
    APP,
}

// This is the raw term tree, where indices are stored like DeBruijn
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TermTree {
    SET(),
    VAR(isize), // DeBruijn index
    LAM(Arc<TermTree>, Arc<TermTree>),
    FOR(Arc<TermTree>, Arc<TermTree>),
    APP(Arc<TermTree>, Arc<TermTree>),
    ERR(String),
}

impl TermTree {
    pub fn offset(&self, offset: isize) -> Arc<TermTree> {
        Arc::new(match self {
            TermTree::SET() => TermTree::SET(),
            TermTree::VAR(x) => TermTree::VAR(x - offset),
            TermTree::LAM(a, b) => TermTree::LAM(a.offset(offset), b.offset(offset)),
            TermTree::FOR(a, b) => TermTree::FOR(a.offset(offset), b.offset(offset)),
            TermTree::APP(a, b) => TermTree::APP(a.offset(offset), b.offset(offset)),
            TermTree::ERR(x) => TermTree::ERR(x.clone()),
        })
    }

    pub fn apply(&self, data: Arc<TermTree>) -> Arc<TermTree> {
        Arc::new(match self {
            TermTree::SET() => TermTree::SET(),
            TermTree::VAR(x) => TermTree::VAR(x - 1),
            TermTree::LAM(a, b) => TermTree::LAM(a.apply(data), b.apply(data)),
            TermTree::FOR(a, b) => TermTree::FOR(a.apply(data), b.apply(data)),
            TermTree::APP(a, b) => TermTree::APP(a.apply(data), b.apply(data)),
            TermTree::ERR(x) => TermTree::ERR(x.clone()),
        })
    }
}

// For LAM and FOR
#[derive(Debug, Clone)]
pub struct TermTreeEvaluatedBody {
    pub typ: Arc<TermTreeEvaluated>, // variables stored as index starting from context root
    pub bod: Arc<TermTree>, // stored as-is, typechecked
    pub ctx: Context, // new context for body
}

#[derive(Debug, Clone)]
pub enum TermTreeEvaluated {
    SET(),
    VAR(usize), // index with respect to context
    LAM(TermTreeEvaluatedBody),
    FOR(TermTreeEvaluatedBody),
    // in APP, both terms are totally evaluated and type checked
    // but their type could still be substituted later
    APP(Arc<TermTreeEvaluated>, Arc<TermTreeEvaluated>),
    ERR(String),
}

pub fn check(a: &Arc<TermTreeEvaluated>, b: &Arc<TermTreeEvaluated>, d: usize) -> Result<(), String> {
    match (Deref::deref(a), Deref::deref(b)) {
        (TermTreeEvaluated::SET(), TermTreeEvaluated::SET()) => Ok(()),
        (TermTreeEvaluated::VAR(x), TermTreeEvaluated::VAR(y)) => {
            if x == y {
                Ok(())
            } else {
                Err(format!("index mismatch: {} != {}", x, y))
            }
        },
        (TermTreeEvaluated::LAM(a), TermTreeEvaluated::LAM(b)) => {
            check(&a.typ, &b.typ, d)?;
            check(&a.bod.eval(a.ctx)?, &b.bod.eval(b.ctx)?, d+1)
        },
        (TermTreeEvaluated::FOR(a), TermTreeEvaluated::FOR(b)) => {
            check(&a.typ, &b.typ, d)?;
            check(&a.bod.eval(a.ctx), &b.bod.eval(b.ctx), d+1)
        },
        (TermTreeEvaluated::APP(aa, ab), TermTreeEvaluated::APP(ba, bb)) => {
            check(aa, ba, d)?;
            check(ab, bb, d)
        }
        _ => false,
    }
}

#[derive(Debug, Clone)]
pub struct Context(Option<Arc<ContextInner>>);

// in Context, variables are stored as index starting from context root
#[derive(Debug, Clone)]
pub struct ContextInner {
    prev: Context,
    i: usize,
    typ: Arc<TermTreeEvaluated>,
}

impl Context {
    pub fn extend(self, typ: Arc<TermTreeEvaluated>) -> Self {
        Context(Some(Arc::new(ContextInner {
            prev: self,
            i: self.id(),
            typ,
        })))
    }

    pub fn id(&self) -> usize {
        match self.0 {
            None => 0,
            Some(x) => x.i+1,
        }
    }

    pub fn get_type(&self, i: usize) -> Option<Arc<TermTreeEvaluated>> {
        match self.0 {
            None => None,
            Some(x) => {
                if x.i < i {
                    None
                } else if x.i == i {
                    Some(x.typ)
                } else {
                    x.prev.get_type(i)
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct TermTreeIncomplete {
    prev: Option<Arc<TermTreeIncomplete>>,
    state: TermParserState,
    ctx: Context,
}

impl std::fmt::Debug for TermTreeIncomplete {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TermParser").field("state", &self.state).field("ctx", &self.ctx).finish()?;
        writeln!(f)?;
        if let Some(prev) = &self.prev {
            std::fmt::Debug::fmt(prev, f)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
enum TermParserState {
    None,
    LAMType,
    LAMValue(Arc<TermTree>), // evaluated TermTree
    FORType,
    FORValue(Arc<TermTree>),
    APPFun,
    // type and body of left arg, both evaluated
    APPArg(Arc<TermTreeEvaluated>, Arc<TermTreeEvaluated>),
    Complete(TermTreeEvaluatedBody),
}

pub fn next_token(s: &mut std::str::Chars<'_>) -> Option<Token> {
    match s.next() {
        None => None,
        Some('#') => Some(Token::SET),
        Some('λ') => Some(Token::LAM),
        Some('∀') => Some(Token::FOR),
        Some('%') => Some(Token::APP),
        Some(x) if x >= 'a' && x <= 'z' => {
            let mut ind: usize = (x as usize) - ('a' as usize);
            let mut b: usize = 1;
            loop {
                if let Some(x) = s.next() {
                    if x >= 'a' && x <= 'z' {
                        b = b * 26;
                        ind += b * ((x as usize) - ('a' as usize));
                    } else if x == ';' {
                        break Some(Token::VAR(ind))
                    } else {
                        break None
                    }
                } else {
                    break None
                }
            }
        },
        _ => None,
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

impl TermTree {
    pub fn iter_tokens(&self) -> impl Iterator<Item = Token> + '_ {
        TermTreeIter(vec![self])
    }

    pub fn eval(self: &Arc<Self>, ctx: Context) -> Result<TermTreeEvaluatedBody, String> {
        let mut tt = Arc::new(TermTreeIncomplete { prev: None, state: TermParserState::None, ctx: Context(None) });
        for token in self.iter_tokens() {
            if let Some(x) = tt.next(token)? {
                return Ok(x);
            }
        }
        return Err("eval TermTree but not complete".to_string());
    }
}

impl TermTreeIncomplete {
    pub fn new() -> Self {
        Self { prev: None, state: TermParserState::None, ctx: Context(None) }
    }
    pub fn next(self: &mut Arc<Self>, tok: Token) -> Result<Option<TermTreeEvaluatedBody>, String> {
        let state = match tok {
            Token::SET => TermParserState::Complete(TermTreeEvaluatedBody {
                typ: Arc::new(TermTreeEvaluated::SET()),
                bod: Arc::new(TermTree::SET()),
                ctx: self.ctx,
            }),
            Token::VAR(n) => {
                let id = self.ctx.id();
                if id < n+1 {
                    return Err("DeBruijn index out of range".to_owned());
                } else {
                    let typ = self.ctx.get_type(id-1-n).unwrap();
                    TermParserState::Complete(TermTreeEvaluatedBody {
                        typ,
                        bod: Arc::new(TermTree::VAR(n)),
                        ctx: self.ctx,
                    })
                }
            },
            Token::APP => TermParserState::APPFun,
            Token::FOR => TermParserState::FORType,
            Token::LAM => TermParserState::LAMType,
        };
        *self = Arc::new(TermTreeIncomplete { prev: Some(*self), state, ctx: self.ctx.clone() });
        while self.reduce()? {};
        if let TermParserState::Complete(res) = &self.prev.as_ref().unwrap().state {
            let res = res.clone();
            let stack = self.prev.as_ref().unwrap().prev.clone();
            self.prev = stack;
            Ok(Some(res))
        } else {
            Ok(None)
        }
    }

    fn reduce(self: &mut Arc<Self>) -> Result<bool, String> {
        let TermTreeIncomplete {
            ctx,
            prev: Some(prev),
            state: TermParserState::Complete(top)
        } = Deref::deref(self) else { return Ok(false) };
        let (newstate, newctx) = match &prev.state {
            TermParserState::None => return Ok(false),
            TermParserState::APPFun => {
                if let TermTreeEvaluated::FOR(funt) = Deref::deref(&top.typ) {
                    let body = top.bod.eval(self.ctx)?;
                    (TermParserState::APPArg(top.typ, body), self.ctx)
                    // (TermParserState::APPArg(top.clone()), ctx)
                } else {
                    return Err(format!("Left side of APP not a function: {:?}", top));
                }
            },
            TermParserState::APPArg(funt, funv) => {
                let (funt, funv) = fun;
                let (argt, argv) = top;
                if let TermTreeLazy::FOR(funt) = Deref::deref(funt) {
                    if check(&funt.typ, &top.typ, cur.next_ctx.id()) {
                        let val = if let TermTreeLazy::LAM(funv) = Deref::deref(funv) {
                            funv.apply(argv.clone())
                        } else {
                            Arc::new(TermTreeLazy::APP(funv.clone(), argv.clone()))
                        };
                        (TermParserState::Complete((funt.apply(argv.clone()), val)), cur.next_ctx.clone())
                    } else {
                        let err = Arc::new(TermTreeLazy::ERR(format!("Type mismatch for APP: funt {:?}, argt {:?}", funt, argt)));
                        (TermParserState::Complete((err.clone(), err)), cur.next_ctx.clone())
                    }
                } else {
                    unreachable!()
                }
            },
            TermParserState::FORType => {
                let (typt, typv) = top;
                if check(&typt, &Arc::new(TermTreeLazy::SET()), 0) {
                    (TermParserState::FORValue(top.clone()), cur.next_ctx.extend(Arc::new(TermTreeLazy::SET()), typv.clone()))
                } else {
                    let err = Arc::new(TermTreeLazy::ERR(format!("In FOR, type is not of type Set: {:?}", top)));
                    (TermParserState::Complete((err.clone(), err)), cur.next_ctx.clone())
                }
            },
            TermParserState::FORValue(typ) => {
                let (typt, typv) = typ;
                let (bodt, bodv) = top;
                if check(&bodt, &Arc::new(TermTreeLazy::SET()), 0) {
                    (TermParserState::Complete((
                        Arc::new(TermTreeLazy::SET()),
                        Arc::new(TermTreeLazy::FOR(TermTreeEvaluatedBody { typ: typv.clone(), bod: bodv.quote(cur.next_ctx.id()), mode: Mode::Value, ctx: cur.next_ctx.clone() }))
                    )), cur.next_ctx.clone())
                } else {
                    let err = Arc::new(TermTreeLazy::ERR(format!("In FOR, body is not of type Set: {:?}", top)));
                    (TermParserState::Complete((err.clone(), err)), cur.next_ctx.clone())
                }
            },
            TermParserState::LAMType => {
                let (typt, typv) = top;
                (TermParserState::LAMValue(top.clone()), cur.next_ctx.clone())
            },
            TermParserState::LAMValue(typ) => {
                let (typt, typv) = typ;
                let (bodt, bodv) = top;
                (TermParserState::Complete((
                    Arc::new(TermTreeLazy::FOR(TermTreeEvaluatedBody { typ: typv.clone(), bod: bodv.quote(cur.next_ctx.id()), mode: Mode::Type, ctx: cur.next_ctx.clone() })),
                    Arc::new(TermTreeLazy::LAM(TermTreeEvaluatedBody { typ: typv.clone(), bod: bodv.quote(cur.next_ctx.id()), mode: Mode::Value, ctx: cur.next_ctx.clone() })),
                )), cur.next_ctx.clone())
            },
            TermParserState::Complete(_) => unimplemented!(),
        };
        self.prev = Some(Arc::new(TermParserStack {
            state: newstate,
            next_ctx: newctx,
            prev: cur.prev.clone(),
        }));
        true
    }
}