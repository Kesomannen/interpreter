use std::collections::HashMap;

use crate::span::Span;

use super::{Call, Error, Executor, Expr, ExprKind, Result, Scope, Type, Value};

struct Values<'a, 'b> {
    executor: &'a mut Executor,
    exprs: std::slice::Iter<'b, Expr>,
}

impl Iterator for Values<'_, '_> {
    type Item = Result<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.exprs.next().map(|expr| self.executor.eval_expr(expr))
    }
}

trait FromValues
where
    Self: Sized,
{
    fn from_values(values: Values) -> Result<Self>;
}

trait FromValue
where
    Self: Sized,
{
    fn from_value(value: Value) -> Result<Self>;
}

impl<T: From<Value>> FromValue for T {
    fn from_value(value: Value) -> Result<Self> {
        Ok(Self::from(value))
    }
}

impl FromValue for i32 {
    fn from_value(value: Value) -> Result<Self> {
        match value {
            Value::Int(int) => Ok(int),
            _ => Err(Error::TypeMismatch {
                expected: Type::Int,
                actual: value,
            }),
        }
    }
}

impl FromValues for () {
    fn from_values(values: Values) -> Result<Self> {
        match values.count() {
            0 => Ok(()),
            count => Err(Error::ArgumentMismatch {
                expected: 0,
                actual: count,
            }),
        }
    }
}

macro_rules! count_tts {
    () => { 0 };
    ($odd:tt $($a:tt $b:tt)*) => { (count_tts!($($a)*) << 1) | 1 };
    ($($a:tt $even:tt)*) => { count_tts!($($a)*) << 1 };
}

macro_rules! tuple_from_values_impl {
    ( $( $x:ident ),* ) => {
        #[allow(unused_parens)]
        impl<$( $x:FromValue ),*> FromValues for ($( $x ),*) {
            fn from_values(mut values: Values) -> Result<Self> {
                let expected = count_tts!($( $x ),*);
                let mut count = 0;

                $(
                    #[allow(non_snake_case)]
                    let $x = $x::from_value(values.next().ok_or(Error::ArgumentMismatch {
                        expected,
                        actual: count,
                    })??)?;

                    count += 1;
                )*

                match values.next() {
                    None => Ok(($( $x ),*)),
                    Some(_) => Err(Error::ArgumentMismatch {
                        expected,
                        actual: 1 + count + values.count(),
                    }),
                }
            }
        }
    };
}

tuple_from_values_impl!(T1);
tuple_from_values_impl!(T1, T2);
tuple_from_values_impl!(T1, T2, T3);
tuple_from_values_impl!(T1, T2, T3, T4);
tuple_from_values_impl!(T1, T2, T3, T4, T5);
tuple_from_values_impl!(T1, T2, T3, T4, T5, T6);
tuple_from_values_impl!(T1, T2, T3, T4, T5, T6, T7);
tuple_from_values_impl!(T1, T2, T3, T4, T5, T6, T7, T8);
tuple_from_values_impl!(T1, T2, T3, T4, T5, T6, T7, T8, T9);

type Builtin = Box<dyn Fn(Values) -> Result<Value>>;

fn f<A, F>(exec: F) -> Builtin
where
    A: FromValues,
    F: Fn(A) -> Result<Value> + 'static,
{
    Box::new(move |values| exec(A::from_values(values)?))
}

impl Executor {
    pub fn eval_call(&mut self, call: &Call, span: Span) -> Result<Value> {
        let builtins: HashMap<&'static str, Builtin> = HashMap::from([
            (
                "print",
                f(|text: Value| {
                    print!("{text}");
                    Ok(Value::Void)
                }),
            ),
            (
                "println",
                f(|text: Value| {
                    println!("{text}");
                    Ok(Value::Void)
                }),
            ),
            (
                "input",
                f(|()| {
                    let mut str = String::new();
                    std::io::stdin().read_line(&mut str).ok();
                    str = str.trim().to_owned();
                    Ok(Value::String(str))
                }),
            ),
            ("exit", f(|()| Err(Error::Exit))),
        ]);

        if let Ok(Value::Func(func)) = self.eval_expr(&call.func) {
            let expected = func.args.len();
            let actual = call.args.len();

            if actual != expected {
                return Err(Error::ArgumentMismatch { expected, actual });
            }

            let vars = func
                .args
                .iter()
                .cloned()
                .zip(self.values(&call.args))
                .map(|(name, value)| value.map(|value| (name, value)))
                .collect::<Result<_>>()?;

            self.scopes.push(Scope { vars });
            let res = self.eval_expr(&func.body)?;
            self.scopes.pop();

            return Ok(res);
        }

        if let ExprKind::Ident(name) = &call.func.kind {
            match builtins.get(name.as_str()) {
                Some(func) => func(self.values(&call.args)),
                None => Err(Error::UndefinedFunction(name.clone(), span)),
            }
        } else {
            Err(Error::CannotInvoke)
        }
    }

    fn values<'a, 'b>(&'a mut self, exprs: &'b [Expr]) -> Values<'a, 'b> {
        Values {
            executor: self,
            exprs: exprs.iter(),
        }
    }
}
