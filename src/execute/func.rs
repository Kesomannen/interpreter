use std::sync::Arc;

use derive_more::derive::Display;

use crate::span::Span;

use super::{Call, Error, Executor, Expr, Func, Result, Scope, Type, Value};

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
                actual: value.ty(),
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

#[derive(Clone)]
pub struct Builtin {
    name: &'static str,
    exec: Arc<dyn Fn(Values) -> Result<Value>>,
}

impl Builtin {
    pub fn name(&self) -> &'static str {
        self.name
    }
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

fn builtin<A, F>(name: &'static str, exec: F) -> Builtin
where
    A: FromValues,
    F: Fn(A) -> Result<Value> + 'static,
{
    Builtin {
        name,
        exec: Arc::new(move |values| exec(A::from_values(values)?)),
    }
}

#[derive(Display, Clone, PartialEq)]
pub enum RuntimeFunc {
    #[display("<builtin func>")]
    Builtin(Builtin),
    #[display("<user func>")]
    User {
        func: Arc<Func>,
        captures: Arc<[(String, Value)]>,
    },
}

impl std::fmt::Debug for RuntimeFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin(_) => f.debug_tuple("Builtin").finish(),
            Self::User { func, captures } => f
                .debug_struct("User")
                .field("func", func)
                .field("captures", captures)
                .finish(),
        }
    }
}

pub fn builtins() -> Vec<Builtin> {
    vec![
        builtin("print", |text: Value| {
            print!("{text}");
            Ok(Value::Void)
        }),
        builtin("println", |text: Value| {
            println!("{text}");
            Ok(Value::Void)
        }),
        builtin("input", |()| {
            let mut str = String::new();
            std::io::stdin().read_line(&mut str).ok();
            str = str.trim().to_owned();
            Ok(Value::String(str))
        }),
        builtin("exit", |()| Err(Error::Exit)),
    ]
}

impl Executor {
    pub fn eval_call(&mut self, call: &Call, _span: Span) -> Result<Value> {
        match self.eval_expr(&call.func)? {
            Value::Func(RuntimeFunc::Builtin(builtin)) => (builtin.exec)(self.values(&call.args)),
            Value::Func(RuntimeFunc::User { func, captures }) => {
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
                    .chain(
                        captures
                            .iter()
                            .map(|(name, value)| Ok((name.clone(), value.clone()))),
                    )
                    .collect::<Result<_>>()?;

                self.scopes.push(Scope { vars });
                let res = self.eval_expr(&func.body)?;
                self.scopes.pop();

                Ok(res)
            }
            value => Err(Error::CannotInvoke(value.ty())),
        }
    }

    fn values<'a, 'b>(&'a mut self, exprs: &'b [Expr]) -> Values<'a, 'b> {
        Values {
            executor: self,
            exprs: exprs.iter(),
        }
    }
}
