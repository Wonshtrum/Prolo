use std::{collections::HashMap, fmt};

#[derive(Clone)]
enum Term {
    Value(Value),
    Variable(String),
}

#[derive(Clone)]
enum Value {
    Number(i64),
    Name(&'static str),
}

#[derive(Clone)]
struct Fact {
    name: String,
    args: Vec<Term>,
}

#[derive(Clone)]
struct Rule {
    head: Fact,
    body: Vec<Fact>,
}

#[derive(Clone)]
struct Interpreter {
    rules: HashMap<String, Vec<Rule>>,
}

impl From<&str> for Term {
    fn from(s: &str) -> Self {
        Term::Variable(s.to_string())
    }
}
impl From<i64> for Term {
    fn from(n: i64) -> Self {
        Term::Value(Value::Number(n))
    }
}

type Bindings = HashMap<String, Value>;

impl Term {
    fn eval(&self, bindings: &Bindings) -> Term {
        match self {
            Term::Value(v) => Term::Value(v.clone()),
            Term::Variable(v) => match bindings.get(v) {
                None => Term::Variable(v.clone()),
                Some(v) => Term::Value(v.clone()),
            },
        }
    }

    fn unify(&self, other: &Term, bindings: &mut Bindings) -> bool {
        match (self.eval(bindings), other.eval(bindings)) {
            (Term::Variable(v1), Term::Variable(v2)) => v1 == v2,
            (Term::Value(Value::Name(n1)), Term::Value(Value::Name(n2))) => n1 == n2,
            (Term::Value(Value::Number(n1)), Term::Value(Value::Number(n2))) => n1 == n2,
            (Term::Variable(v), Term::Value(n)) | (Term::Value(n), Term::Variable(v)) => {
                bindings.insert(v, n);
                true
            }
            // mismatched Value types
            _ => false,
        }
    }
}

impl Fact {
    fn unify(&self, other: &Fact, bindings: &mut Bindings) -> bool {
        self.name == other.name
            && self
                .args
                .iter()
                .zip(&other.args)
                .all(|(a, b)| a.unify(b, bindings))
    }

    fn eval(&self, bindings: &Bindings) -> Fact {
        Fact {
            name: self.name.clone(),
            args: self.args.iter().map(|arg| arg.eval(bindings)).collect(),
        }
    }
}

fn repeat(pattern: &str, n: usize) -> String {
    (0..n).map(|_| pattern).collect()
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            rules: HashMap::new(),
        }
    }

    fn add_rule(&mut self, rule: Rule) {
        let head_name = rule.head.name.clone();
        self.rules
            .entry(head_name)
            .or_insert_with(Vec::new)
            .push(rule);
    }

    fn query(&self, fact: &Fact) -> bool {
        let mut bindings = HashMap::new();
        self._query(fact, &mut bindings, 0)
    }

    fn _query(&self, fact: &Fact, bindings: &mut Bindings, depth: usize) -> bool {
        println!("{}QUERY: {:?}", repeat("│ ", depth), fact);
        if let Some(rules) = self.rules.get(&fact.name) {
            for rule in rules {
                let mut rule_bindings = HashMap::new();
                if rule.head.unify(fact, &mut rule_bindings)
                    && rule.body.iter().all(|fact| {
                        self._query(&fact.eval(&rule_bindings), &mut rule_bindings, depth + 1)
                    })
                {
                    bindings.extend(rule_bindings);
                    println!(
                        "{}└ true because {:?} with: {:?}",
                        repeat("│ ", depth),
                        rule,
                        bindings
                    );
                    return true;
                }
            }
        }
        println!("{}└ false", repeat("│ ", depth));
        false
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Value(v) => write!(f, "{:?}", v),
            Term::Variable(v) => write!(f, "{}", v),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Name(n) => write!(f, "{:?}", n),
        }
    }
}

impl fmt::Debug for Fact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{:?}", self.name, self.args)
    }
}

impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.head)?;
        if !self.body.is_empty() {
            write!(f, " :- ")?;
            for (i, term) in self.body.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{:?}", term)?;
            }
        }
        write!(f, ".")
    }
}

impl fmt::Debug for Interpreter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for rules in self.rules.values() {
            for rule in rules {
                writeln!(f, "{:?}", rule)?;
            }
        }
        Ok(())
    }
}

macro_rules! fact {
     ($name:literal $(( $($args:expr),+ ))?) => {
        Fact {
            name: $name.to_string(),
            args: vec![$($($args.into()),+)?],
        }
     };
}

macro_rules! rule {
    ($head_name:literal $(( $($head_args:expr),+ ))? $( :- $(
     $body_name:literal $(( $($body_args:expr),+ ))?),+ )?) => {
        Rule {
            head: Fact {
                name: $head_name.to_string(),
                args: vec![$($($head_args.into()),+)?],
            },
            body: vec![$($(
                Fact {
                    name: $body_name.to_string(),
                    args: vec![$($($body_args.into()),+)?],
                }
            ),+)?],
        }
    };
}

fn main() {
    let mut interpreter = Interpreter::new();

    interpreter.add_rule(rule!("even"("X") :- ">="("X", 0), "mod"("X", 2, 0)));
    interpreter.add_rule(rule!("a1"(3, 4)));
    interpreter.add_rule(rule!("a1"(1, 2)));
    interpreter.add_rule(rule!("a1"(2, 3)));
    interpreter.add_rule(rule!("a2"("X", "Y") :- "a1"("X", "Z"), "a1"("Z", "Y")));
    interpreter.add_rule(rule!("an"("X", "X")));
    interpreter.add_rule(rule!("an"("X", "Y") :- "a1"("X", "Z"), "an"("Z", "Y")));

    println!("{:?}", interpreter);

    let query1 = fact!("even"(4));
    let query2 = fact!("even"(5));
    let query3 = fact!("a2"(1, 3));
    let query4 = fact!("a2"(1, 4));
    let query5 = fact!("an"(1, 4));
    let query6 = fact!("an"(2, 1));

    let f1 = fact!("a"(1, 2));
    let f2 = fact!("a"("X", "Y"));
    let mut bindings = HashMap::new();
    println!("{}", f1.unify(&f2, &mut bindings));
    println!("{:?}", bindings);
    let f1 = fact!("a"(1, 3));
    println!("{}", f1.unify(&f2, &mut bindings));
    println!("{:?}\n", bindings);

    println!("{:?}?- {}\n", query1, interpreter.query(&query1));
    println!("{:?}?- {}\n", query2, interpreter.query(&query2));
    println!("{:?}?- {}\n", query3, interpreter.query(&query3));
    println!("{:?}?- {}\n", query4, interpreter.query(&query4));
    println!("{:?}?- {}\n", query5, interpreter.query(&query5));
    println!("{:?}?- {}\n", query6, interpreter.query(&query6));
}
