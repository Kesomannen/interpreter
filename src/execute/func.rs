use std::collections::HashMap;

use crate::span::Span;

use super::{Call, Error, Executor, Expr, Result, Type, Value};

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

type Func = Box<dyn Fn(Values) -> Result<Value>>;

fn f<A, F>(exec: F) -> Func
where
    A: FromValues,
    F: Fn(A) -> Result<Value> + 'static,
{
    Box::new(move |values| exec(A::from_values(values)?))
}

impl Executor {
    pub fn eval_call(&mut self, call: &Call, span: Span) -> Result<Value> {
        let funcs: HashMap<&'static str, Func> = HashMap::from([
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

        let values = Values {
            executor: self,
            exprs: call.args.iter(),
        };

        match funcs.get(call.name.as_str()) {
            Some(func) => func(values),
            None => Err(Error::UndefinedFunction(call.name.clone(), span)),
        }
    }
}
