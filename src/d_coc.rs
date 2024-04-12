use std::sync::Arc;

// in Token, variables are stored as DeBruijn index
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token {
    SET,
    VAR(usize),
    LAM,
    FOR,
    APP,
}

fn var_name(mut x: usize, s: &mut String)  {
    loop {
        s.push(('a' as u8 + (x%26) as u8) as char);
        x /= 26;
        if x == 0 { break }
    }
}

impl Token {
    pub fn append_to(&self, str: &mut String) {
        match self {
            Token::SET => str.push('#'),
            Token::VAR(x) => { var_name(*x, str); str.push(';'); },
            Token::LAM => str.push('λ'),
            Token::FOR => str.push('∀'),
            Token::APP => str.push('%'),
        }
    }
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

// This is the raw term tree, where indices are stored like DeBruijn
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TermTree {
    SET(),
    VAR(usize), // DeBruijn index
    LAM(Arc<TermTree>, Arc<TermTree>),
    FOR(Arc<TermTree>, Arc<TermTree>),
    APP(Arc<TermTree>, Arc<TermTree>),
}
struct TermTreeIter<'a>(Vec<&'a TermTree>);
impl Iterator for TermTreeIter<'_> {
    type Item = Token;
    
    fn next(&mut self) -> Option<Self::Item> {
        match self.0.pop() {
            Some(TermTree::SET()) => Some(Token::SET),
            Some(TermTree::VAR(x)) => Some(Token::VAR(*x)),
            Some(TermTree::LAM(x, y)) => { self.0.extend([&**y, &**x].iter()); Some(Token::LAM) },
            Some(TermTree::FOR(x, y)) => { self.0.extend([&**y, &**x].iter()); Some(Token::FOR) },
            Some(TermTree::APP(x, y)) => { self.0.extend([&**y, &**x].iter()); Some(Token::APP) },
            None => None,
        }
    }
}

impl TermTree {
    pub fn offset_ij(&self, i: usize, offset: usize) -> Arc<TermTree> {
        match self {
            TermTree::SET() => Arc::new(TermTree::SET()),
            TermTree::VAR(x) => {
                if *x >= i {
                    Arc::new(TermTree::VAR(x + offset))
                } else {
                    Arc::new(TermTree::VAR(*x))
                }
            },
            TermTree::LAM(a, b) => Arc::new(TermTree::LAM(a.offset_ij(i, offset), b.offset_ij(i, offset))),
            TermTree::FOR(a, b) => Arc::new(TermTree::FOR(a.offset_ij(i, offset), b.offset_ij(i, offset))),
            TermTree::APP(a, b) => Arc::new(TermTree::APP(a.offset_ij(i, offset), b.offset_ij(i, offset))),
        }
    }

    pub fn reverse(&self, offset: usize) -> Arc<TermTree> {
        match self {
            TermTree::SET() => Arc::new(TermTree::SET()),
            TermTree::VAR(x) => Arc::new(TermTree::VAR(offset-1-x)),
            TermTree::LAM(a, b) => Arc::new(TermTree::LAM(a.reverse(offset), b.reverse(offset+1))),
            TermTree::FOR(a, b) => Arc::new(TermTree::FOR(a.reverse(offset), b.reverse(offset+1))),
            TermTree::APP(a, b) => Arc::new(TermTree::APP(a.reverse(offset), b.reverse(offset))),
        }
    }

    // Apply to TermTree with absolute offsets
    pub fn apply(&self, data: Arc<TermTree>, absolute_offset: usize) -> Arc<TermTree> {
        match self {
            TermTree::SET() => Arc::new(TermTree::SET()),
            TermTree::VAR(x) => match (*x).cmp(&absolute_offset) {
                std::cmp::Ordering::Less => Arc::new(TermTree::VAR(*x)),
                std::cmp::Ordering::Equal => data,
                std::cmp::Ordering::Greater => Arc::new(TermTree::VAR(x-1)),
            }
            // TermTree::VAR(x) => Arc::new(TermTree::VAR(x - 1)),
            TermTree::LAM(a, b) => Arc::new(TermTree::LAM(a.apply(data.clone(), absolute_offset), b.apply(data, absolute_offset))),
            TermTree::FOR(a, b) => Arc::new(TermTree::FOR(a.apply(data.clone(), absolute_offset), b.apply(data, absolute_offset))),
            TermTree::APP(a, b) => Arc::new(TermTree::APP(a.apply(data.clone(), absolute_offset), b.apply(data, absolute_offset))),
        }
    }

    pub fn iter_tokens(&self) -> impl Iterator<Item = Token> + '_ {
        TermTreeIter(vec![self])
    }

    pub fn to_string(&self) -> String {
        let mut s = String::new();
        self.iter_tokens().for_each(|tok| tok.append_to(&mut s));
        s
    }
}

#[derive(Debug, Clone)]
enum TermParserState {
    LAMType,
    LAMValue(Arc<TermTree>),
    FORType,
    FORValue(Arc<TermTree>, Context),
    APPFun,
    // type and body of left arg, both evaluated
    APPArg(Arc<TermTree>, Arc<TermTree>, Arc<TermTree>), // argtype, rettype, function body
    Complete(Arc<TermTree>, Arc<TermTree>),
}

#[derive(Clone)]
pub struct Context(Option<Arc<ContextInner>>);

#[derive(Debug, Clone)]
pub struct ContextInner {
    prev: Context,
    typ: Arc<TermTree>,
    i: usize,
}

impl Context {
    fn get_inner(&self, i: usize) -> Option<Arc<TermTree>> {
        match &self.0 {
            None => None,
            Some(x) => {
                if x.i == i {
                    Some(x.typ.clone())
                } else if x.i < i {
                    None
                } else {
                    x.prev.get_inner(i)
                }
            }
        }
    }

    pub fn get(&self, i: usize) -> Option<Arc<TermTree>> {
        match self.get_inner(i) {
            None => None,
            Some(x) =>  {
                let y = Some(x.offset_ij(i, self.id()-i));
                println!("bump {:?} {} {} => {:?}", x, i, self.id()-i, y);
                y
            },
        }
    }

    pub fn id(&self) -> usize {
        match &self.0 {
            None => 0,
            Some(x) => x.i+1,
        }
    }
    pub fn extend(&self, typ: Arc<TermTree>) -> Context {
        let i = self.id(); 
        Context(Some(Arc::new(ContextInner {
            prev: self.clone(),
            typ,
            i,
        })))
    }
}

impl std::fmt::Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            // Self::Tail => write!(f, "Tail"),
            // Self::Head { typ, bod, next, id } => f.debug_struct("Head").field("typ", typ).field("bod", bod).field("next", next).field("id", id).finish(),
            None => Ok(()),
            Some(ref inner) => {
                let ContextInner { prev, typ, i } = &**inner;
                std::fmt::Debug::fmt(prev, f)?;
                write!(f, "{}:{:?}\n", i, typ)
            }
        }
    }
}

#[derive(Clone)]
pub struct PartialTerm(Option<Arc<PartialTermInner>>);
// Partial term
#[derive(Debug, Clone)]
pub struct PartialTermInner {
    state: TermParserState,
    ctx: Context,
    goal: Option<Arc<TermTree>>,
    prev: PartialTerm,
}

impl std::fmt::Debug for PartialTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            None => writeln!(f, "PartialTerm(None)"),
            Some(ref x) => {
                let mut x = x;
                writeln!(f, "PartialTerm with context:")?;
                write!(f, "{:?}", x.ctx)?;
                writeln!(f, "Goal: {:?}", x.goal)?;
                writeln!(f, "State: {:?}", x.state)?;
                while let Some(xx) = &x.prev.0 {
                    x = xx;
                    writeln!(f, "State: {:?}", x.state)?;
                }
                Ok(())
            }
        }
        // f.debug_tuple("PartialTerm").field(&self.0).finish()
    }
}

impl PartialTerm {
    pub fn empty() -> Self {
        Self(None)
    }
    pub fn ctx(&self) -> Context {
        match &self.0 {
            None => Context(None),
            Some(x) => x.ctx.clone(),
        }
    }
    pub fn goal(&self) -> Option<Arc<TermTree>> {
        match &self.0 {
            None => None,
            Some(x) => x.goal.clone(),
        }
    }
    // fn get(&self, i: usize) -> Option<Arc<TermTree>> {
    //     match self.0 {
    //         None => None,
    //         Some(x) => x.ctx.get(i),
    //     }
    // }
    pub fn next(&mut self, token: Token) -> Result<Option<(Arc<TermTree>, Arc<TermTree>)>, String> {
        let (ctx, goal) = match &self.0 {
            None => (Context(None), None),
            Some(x) => (x.ctx.clone(), x.goal.clone()),
        };
        let state = match token {
            Token::SET => TermParserState::Complete(
                Arc::new(TermTree::SET()),
                Arc::new(TermTree::SET()),
            ),
            Token::VAR(n) => {
                let y = ctx.id() - 1 - n;
                let typ = ctx.get(y);
                // println!("DeBruijn index {}, got {:?}", n, typ);
                match typ {
                    None => return Err(format!("DeBruijn index out of range")),
                    Some(typ) => TermParserState::Complete(
                        typ,
                        Arc::new(TermTree::VAR(y)),
                    )
                }
            },
            Token::APP => TermParserState::APPFun,
            Token::FOR => TermParserState::FORType,
            Token::LAM => TermParserState::LAMType,
        };
        *self = PartialTerm(Some(Arc::new(PartialTermInner {
            state,
            ctx,
            prev: self.clone(),
            goal,
        })));
        while self.reduce()? {};
        if let Some(x) = &self.0 {
            if let TermParserState::Complete(typ, bod) = &x.state {
                println!("{:?}", typ);
                let typ = typ.reverse(0);
                let bod = bod.reverse(0);
                let prev = x.prev.clone();
                *self = prev;
                Ok(Some((typ, bod)))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn reduce(&mut self) -> Result<bool, String> {
        let Some(inner) = &self.0 else { return Ok(false); };
        let PartialTermInner {
            state: TermParserState::Complete(typ, bod),
            ctx: _ctx,
            prev,
            goal: _goal,
        } = &**inner else { return Ok(false); };
        let Some(inner) = &prev.0 else { return Ok(false); };
        let PartialTermInner {
            state,
            ctx,
            prev,
            goal,
        } = &**inner;
        let (newstate, newctx, newgoal) = match state {
            TermParserState::Complete(..) => return Ok(false),
            TermParserState::APPFun => {
                if let TermTree::FOR(argtyp, rettyp) = &**typ {
                    println!("APP: fun goal {:?} ret {:?}", argtyp, rettyp);
                    (TermParserState::APPArg(argtyp.clone(), rettyp.clone(), bod.clone()), ctx.clone(), Some(argtyp.clone()))
                } else {
                    return Err(format!("Left side of APP not a function of type FOR: {:?}", typ));
                }
            },
            TermParserState::APPArg(argtyp, rettyp, funbod) => {
                // let ctx = ctx.apply(typ.clone())?;
                if argtyp != typ {
                    return Err(format!("APP type mismatch: expected {:?}, got {:?}", argtyp, typ));
                }
                let rettyp = rettyp.apply(bod.clone(), ctx.id());
                let resbod = if let TermTree::LAM(_, funbod) = &**funbod {
                    // TODO: How to do this apply?
                    funbod.apply(bod.clone(), ctx.id())
                } else {
                    Arc::new(TermTree::APP(funbod.clone(), bod.clone()))
                };
                println!("APP success: expected typ {:?} cur typ {:?} funbod {:?} bod {:?} res {:?} restyp {:?}", argtyp, typ, funbod, bod, resbod, rettyp);
                (TermParserState::Complete(rettyp.clone(), resbod), ctx.clone(), goal.clone())
            },
            TermParserState::FORType => {
                if let TermTree::SET() = &**typ {
                    // NOT typ.clone()!
                    (TermParserState::FORValue(bod.clone(), ctx.clone()), ctx.extend(bod.clone()), Some(Arc::new(TermTree::SET())))
                } else {
                    return Err(format!("Left side of FOR not of type SET: {:?}", typ));
                }
            },
            TermParserState::FORValue(typbod, prevctx) => {
                if let TermTree::SET() = &**typ {
                    (TermParserState::Complete(
                        Arc::new(TermTree::SET()),
                        Arc::new(TermTree::FOR(typbod.clone(), bod.clone())),
                    ), prevctx.clone(), None)
                } else {
                    return Err(format!("Right side of FOR not of type SET: {:?}", typ));
                }
            },
            TermParserState::LAMType => {
                if let TermTree::SET() = &**typ {
                    // Goal: TODO
                    (TermParserState::LAMValue(bod.clone()), ctx.extend(bod.clone()), None)
                } else {
                    return Err(format!("Left side of APP not a function of type FOR: {:?}", typ));
                }
            },
            TermParserState::LAMValue(typbod) => {
                // TODO: this type maybe not correct, we need to take out var0 in typ
                // (x:* y:x y) : (a.* (.a a))
                // λ#λa;a; : ∀#∀a;b;
                // TODO: LAM?
                println!("Lambda complete: type {:?} val type {:?} body {:?}", typbod.to_string(), typ.to_string(), bod.to_string());
                (TermParserState::Complete(
                    Arc::new(TermTree::FOR(typbod.clone(), typ.clone())),
                    Arc::new(TermTree::LAM(typbod.clone(), bod.clone())),
                ), ctx.clone(), None)
            },
        };
        *self = PartialTerm(Some(Arc::new(PartialTermInner {
            state: newstate,
            ctx: newctx,
            prev: prev.clone(),
            goal: newgoal,
        })));
        Ok(true)
    }
}