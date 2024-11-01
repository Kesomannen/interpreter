# Interpreter

A simple, expressive and dynamically typed interpreted language.

## Language Tour

_Everything_ is an expression.

String, bool and integer literals:

```
3;
true;
"Hello, world";
```

Assign variables with `<name> = <value>`:

```
a = false;
a = "Hello"; // variables are dynamic
```

Group expressions and declarations with blocks:

```
c = {
    a = "wow";
    a; // the last expression is returned
};

c; // "wow"
a; // err: undefined
```

Basic arithmetic:

```
num = 2 + 3 * 5; // 17
num = { 2 + 3 } * 5; // 25
```

Call a function with `name(<args>)`

```
name = input();
println("Hello " + name);
```

Create a function with `|<args>| <body>`:

```
double = |x| x * 2;
double(4); // 8
```

Functions can return functions as well as capture the environment:

```
multiply_by = |factor| {
    |x| x * factor; // captures factor
};

triple = multiply_by(3);
triple(5); // 15
```

Run conditional code with `if` and `else`:

```
balance = 15;

msg = if (balance >= 100) {
    "you're rich!";
} else if (balance < 0) {
    "you owe some";
} else "you're doing fine";

println(msg);
```
