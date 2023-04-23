use std::{collections::HashMap, fmt};

#[derive(Clone, PartialEq, Eq, Hash)]
struct Literal(&'static str);

#[derive(Clone, PartialEq)]
enum Term {
    Value(Value),
    Variable(Literal),
}

#[derive(Clone, PartialEq)]
enum Value {
    Number(i64),
    Name(Literal),
}

#[derive(Clone)]
struct Fact {
    name: Literal,
    args: Vec<Term>,
    exec: bool,
}

#[derive(Clone)]
struct Rule {
    head: Fact,
    body: Vec<Fact>,
}

#[derive(Clone)]
struct Interpreter {
    rules: HashMap<Literal, Vec<Rule>>,
}

impl From<&'static str> for Term {
    fn from(s: &'static str) -> Self {
        if s.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
            Term::Variable(Literal(s))
        } else {
            Term::Value(Value::Name(Literal(s)))
        }
    }
}
impl From<i64> for Term {
    fn from(n: i64) -> Self {
        Term::Value(Value::Number(n))
    }
}

type Bindings = HashMap<Literal, Value>;

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
            exec: self.exec,
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

    fn add_rules(&mut self, rules: Vec<Rule>) {
        for rule in rules {
            self.add_rule(rule);
        }
    }

    fn query(&self, fact: &Fact) -> bool {
        let mut bindings = HashMap::new();
        self._query(vec![fact.clone()], &mut bindings, 0)
    }

    fn _query(&self, mut facts: Vec<Fact>, bindings: &mut Bindings, depth: usize) -> bool {
        let fact = if facts.is_empty() {
            return true;
        } else {
            facts.remove(0)
        };

        let fact = fact.eval(bindings);
        if fact.exec {
            let args = &fact.args;
            let result = match fact.name.0 {
                "neq" => args[0] != args[1],
                "gte" => match (&args[0], &args[1]) {
                    (Term::Value(Value::Number(n1)), Term::Value(Value::Number(n2))) => n1 >= n2,
                    _ => false,
                },
                "lte" => match (&args[0], &args[1]) {
                    (Term::Value(Value::Number(n1)), Term::Value(Value::Number(n2))) => n1 <= n2,
                    _ => false,
                },
                "mod" => match (&args[0], &args[1], &args[2]) {
                    (
                        Term::Value(Value::Number(n1)),
                        Term::Value(Value::Number(n2)),
                        Term::Value(Value::Number(n3)),
                    ) => n1 % n2 == *n3,
                    _ => false,
                },
                _ => false,
            };
            println!(
                "{}BUILTIN: {:?}:- {} with: {:?}",
                repeat("│ ", depth),
                fact,
                result,
                bindings
            );
            if result {
                return self._query(facts.clone(), bindings, depth + 1);
            }
        }

        println!(
            "{}QUERY: {:?} with: {:?}",
            repeat("│ ", depth),
            fact,
            bindings
        );
        if let Some(rules) = self.rules.get(&fact.name) {
            for rule in rules {
                let mut rule_bindings = HashMap::new();
                if rule.head.unify(&fact, &mut rule_bindings)
                    && self._query(rule.body.clone(), &mut rule_bindings, depth + 1)
                {
                    rule_bindings.extend(bindings.clone());
                    if self._query(facts.clone(), &mut rule_bindings, depth + 1) {
                        *bindings = rule_bindings;
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
        }
        println!("{}└ false", repeat("│ ", depth));
        false
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
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

fn is_builtin(name: &str) -> bool {
    matches!(name, "neq" | "gte" | "lte" | "mod")
}

macro_rules! fact {
     ($name:literal $(( $($args:expr),+ ))?) => {
        Fact {
            name: Literal($name),
            args: vec![$($($args.into()),+)?],
            exec: is_builtin($name),
        }
     };
}

macro_rules! rule {
    ($head_name:literal $(( $($head_args:expr),+ ))? $( :- $(
     $body_name:literal $(( $($body_args:expr),+ ))?),+ )?) => {
        Rule {
            head: Fact {
                name: Literal($head_name),
                args: vec![$($($head_args.into()),+)?],
                exec: false,
            },
            body: vec![$($(
                Fact {
                    name: Literal($body_name),
                    args: vec![$($($body_args.into()),+)?],
                    exec: is_builtin($body_name),
                }
            ),+)?],
        }
    };
}

macro_rules! rules {
    ($(
     $head_name:literal $(( $($head_args:expr),+ ))? $( :- $(
     $body_name:literal $(( $($body_args:expr),+ ))?),+ )?
     ).+ .) => {
        vec![$(
            Rule {
                head: Fact {
                    name: Literal($head_name),
                    args: vec![$($($head_args.into()),+)?],
                    exec: false,
                },
                body: vec![$($(
                    Fact {
                        name: Literal($body_name),
                        args: vec![$($($body_args.into()),+)?],
                        exec: is_builtin($body_name),
                    }
                ),+)?],
            }
        ),+]
    };
}

fn main() {
    let mut interpreter = Interpreter::new();
    interpreter.add_rule(rule!("even"("X") :- "gte"("X", 0), "mod"("X", 2, 0)));
    interpreter.add_rules(rules! {
        "a1"(3, 4).
        "a1"(1, 2).
        "a1"(2, 3).
        "a2"("X", "Y") :- "a1"("X", "Z"), "a1"("Z", "Y").
        "an"("X", "X").
        "an"("X", "Y") :- "a1"("X", "Z"), "an"("Z", "Y").
    });
    interpreter.add_rules(rules! {
        "female"("pam").
        "female"("liz").
        "female"("pat").
        "female"("ann").
        "male"("jim").
        "male"("bob").
        "male"("tom").
        "male"("peter").
        "parent"("pam","bob").
        "parent"("tom","bob").
        "parent"("tom","liz").
        "parent"("bob","ann").
        "parent"("bob","pat").
        "parent"("pat","jim").
        "parent"("bob","peter").
        "parent"("peter","jim").
        "mother"("X","Y"):- "parent"("X","Y"),"female"("X").
        "father"("X","Y"):- "parent"("X","Y"),"male"("X").
        "haschild"("X"):- "parent"("X","Y").
        "sister"("X","Y"):- "parent"("Z","X"),"parent"("Z","Y"),"female"("X"),"neq"("X","Y").
        "brother"("X","Y"):- "parent"("Z","X"),"parent"("Z","Y"),"male"("X"),"neq"("X","Y").
    });

    println!("{:?}", interpreter);

    let query = fact!("even"(4));
    println!("{:?}?- {}\n", query, interpreter.query(&query));
    let query = fact!("even"(5));

    println!("{:?}?- {}\n", query, interpreter.query(&query));
    let query = fact!("a2"(1, 3));
    println!("{:?}?- {}\n", query, interpreter.query(&query));
    let query = fact!("a2"(1, 4));
    println!("{:?}?- {}\n", query, interpreter.query(&query));
    let query = fact!("an"(1, 4));
    println!("{:?}?- {}\n", query, interpreter.query(&query));
    let query = fact!("an"(2, 1));
    println!("{:?}?- {}\n", query, interpreter.query(&query));
    let query = fact!("parent"("X", "jim"));
    println!("{:?}?- {}\n", query, interpreter.query(&query));

    let query = fact!("mother"("X", "Y"));
    println!("{:?}?- {}\n", query, interpreter.query(&query));
    let query = fact!("haschild"("X"));
    println!("{:?}?- {}\n", query, interpreter.query(&query));
    let query = fact!("sister"("X", "Y"));
    println!("{:?}?- {}\n", query, interpreter.query(&query));
}
