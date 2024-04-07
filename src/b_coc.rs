pub enum Term {
    Var(usize), // Variable, represented by De Bruijn indices
    Lam(Box<Term>, Box<Term>), // Lambda abstraction, with parameter type and body
    App(Box<Term>, Box<Term>), // Application, with function and argument
    Pi(Box<Term>, Box<Term>), // Type of functions, with parameter type and return type
    Prop, // The type of all propositions
    Set, // The type of all types
}

impl Term {
    pub fn type_check(&self, context: &Vec<Term>) -> Result<Term, String> {
        match self {
            Term::Var(index) => context.get(*index).cloned().ok_or_else(|| "Variable out of scope".to_string()),
            Term::Lam(param_type, body) => {
                let mut extended_context = context.clone();
                extended_context.push(*param_type.clone());
                let body_type = body.type_check(&extended_context)?;
                Ok(Term::Pi(param_type.clone(), Box::new(body_type)))
            },
            Term::App(func, arg) => {
                let func_type = func.type_check(context)?;
                match *func_type {
                    Term::Pi(param_type, return_type) => {
                        let arg_type = arg.type_check(context)?;
                        if *param_type == arg_type {
                            Ok(*return_type)
                        } else {
                            Err("Type mismatch in application".to_string())
                        }
                    },
                    _ => Err("Application of non-function".to_string()),
                }
            },
            Term::Pi(param_type, return_type) => {
                match (**param_type, return_type.type_check(context)?) {
                    (Term::Prop, Term::Prop) | (Term::Set, Term::Prop) | (Term::Set, Term::Set) => Ok(Term::Set),
                    _ => Err("Invalid Pi type formation".to_string()),
                }
            },
            Term::Prop => Ok(Term::Set),
            Term::Set => Ok(Term::Set),
        }
    }
    
    pub fn eval(term: Term) -> Term {
        match term {
            Term::Var(_) => term, // Variables are already in normal form.
            Term::Lam(param_type, body) => {
                // Lambda abstractions are values and thus already in normal form.
                Term::Lam(param_type, body)
            },
            Term::App(func, arg) => {
                let func_eval = eval(*func);
                match func_eval {
                    Term::Lam(_, body) => {
                        // Beta reduction: replace the body of the lambda with the argument.
                        eval(substitute(*body, 0, &arg))
                    },
                    _ => Term::App(Box::new(func_eval), arg),
                }
            },
            Term::Pi(param_type, return_type) => {
                // Pi types are already in normal form.
                Term::Pi(param_type, return_type)
            },
            Term::Prop => term, // Prop is already in normal form.
            Term::Set => term,  // Set is already in normal form.
        }
    }
    
    
pub fn eval(term: Term) -> Term {
    match term {
        Term::Var(_) => term, // Variables are already in normal form.
        Term::Lam(param_type, body) => {
            // Lambda abstractions are values and thus already in normal form.
            Term::Lam(param_type, body)
        },
        Term::App(func, arg) => {
            let func_eval = eval(*func);
            match func_eval {
                Term::Lam(_, body) => {
                    // Beta reduction: replace the body of the lambda with the argument.
                    eval(substitute(*body, 0, &arg))
                },
                _ => Term::App(Box::new(func_eval), arg),
            }
        },
        Term::Pi(param_type, return_type) => {
            // Pi types are already in normal form.
            Term::Pi(param_type, return_type)
        },
        Term::Prop => term, // Prop is already in normal form.
        Term::Set => term,  // Set is already in normal form.
    }

    pub fn substitute(term: Term, index: usize, value: &Term) -> Term {
        match term {
            Term::Var(i) => {
                if i == index {
                    value.clone() // Replace the variable with the value.
                } else {
                    Term::Var(i) // Different variable, no substitution.
                }
            },
            Term::Lam(param_type, body) => {
                // When entering a lambda, increase the de Bruijn index.
                Term::Lam(param_type.clone(), Box::new(substitute(*body, index + 1, value)))
            },
            Term::App(func, arg) => {
                // Substitute in both function and argument.
                Term::App(Box::new(substitute(*func, index, value)), Box::new(substitute(*arg, index, value)))
            },
            Term::Pi(param_type, return_type) => {
                // Substitute in both parameter type and return type.
                Term::Pi(Box::new(substitute(*param_type, index, value)), Box::new(substitute(*return_type, index + 1, value)))
            },
            Term::Prop => term, // Prop has no variables to substitute.
            Term::Set => term,  // Set has no variables to substitute.
        }
    }
    

    
}
