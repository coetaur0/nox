# :night_with_stars: Nox

**Nox** is a statically typed, functional programming language that looks and feels like a dynamic one thanks to [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) type inference and [row polymorphism](https://en.wikipedia.org/wiki/Row_polymorphism).

Programs written in Nox can be compiled to [Lua](https://www.lua.org/) or directly executed by the language's interpreter.
A [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) is also available to interactively try out the language and its features.

To give you a taste of Nox, here's a very simple example of a program that recursively computes the [factorial](https://en.wikipedia.org/wiki/Factorial) of a number:
```
fun fact(n) {
  if n <= 1 {
    1
  } else {
    n * fact(n - 1)
  }
};

print("The factorial of 5 is " .. num2str(fact(5)) .. ".")
```

- [Installation](#installation)
- [Usage](#usage)
  - [Running the Nox REPL](#running-the-nox-repl)
  - [Running the Nox interpreter](#running-the-nox-interpreter)
  - [Compiling Nox sources to Lua](#compiling-nox-sources-to-lua)
- [A quick tour of the language](#a-quick-tour-of-the-language)
  - [Statements](#statements)
    - [Function declarations](#function-declarations)
    - [Variable definitions](#variable-definitions)
    - [Mutable references](#mutable-references)
    - [While loops](#while-loops)
  - [Expressions](#expressions)
    - [Basic types and operations](#basic-types-and-operations)
    - [Records](#records)
    - [Variants](#variants)
    - [Blocks](#blocks)
    - [Conditional expressions](#conditional-expressions)
    - [Lambda expressions](#lambda-expressions)
    - [Modules](#modules)

---

## Installation

To build and install the Nox compiler and interpreter, you will first need to install the [opam](https://opam.ocaml.org/) package manager for the [OCaml](https://ocaml.org/) programming language on your machine.

Once this is done, simply run the following commands to clone this repository, create a local [opam switch](https://opam.ocaml.org/doc/Usage.html#opam-switch) for the project and install Nox on your computer:

```bash
$ git clone git@github.com:coetaur0/nox.git
$ cd nox
$ opam switch create . 5.0.0
$ dune install
```

---

## Usage

There are three different ways to execute Nox programs:

  - Interactively in the language's [REPL](#running-the-nox-repl);
  - Directly from source with the [interpreter](#running-the-nox-interpreter);
  - By using a Lua interpreter, after having [compiled](#compiling-nox-sources-to-lua) Nox sources to Lua code.

### Running the Nox REPL

To launch the REPL, simply run the `nox` command in your terminal without any arguments.

Inside the REPL, any sequence of Nox [statements](#statements) can be executed by typing it in, ending the sequence with `;;` (two semi-colons) and pressing `Enter`.

As an example, the following snippet launches the REPL, defines a new function `add` and calls it with two arguments:

```
$ nox
Welcome to the Nox REPL.
> fun add(x, y) {
|   x + y
| };
| add(20, 22);;
 42. : number
>
```

After evaluating code, the REPL prints out the return value of the sequence of statements it received as input (`42.` in our example), along with its type (`number`).

In addition to running arbitrary Nox code, the REPL can also execute a couple of predefined commands that aren't part of the language (identified by their `$` prefix):

- Command `$load "/path/to/some/module.nox";;` loads the Nox [module](#modules) in file `module.nox` located at path `/path/to/some/`.
  Loading a module executes the statements it contains and makes it available to be [opened](#modules) in the current REPL environment.

- Command `$quit;;` simply ends the current session and closes the REPL.
  Alternatively, the `ctrl + D` shortcut can be used to the same end.

### Running the Nox interpreter

To directly run the Nox interpreter on source code written in some `.nox` files without going through the REPL, simply call `nox` in your terminal with the paths of the files to execute as arguments.
Note that the order of the arguments is significant here.
If the code in one of your files references another module with an [`open`](#modules) statement, the file containing the module being opened must be passed as argument to the interpreter **before** the one opening it.

Here's an example of calling the Nox interpreter on three modules defined in the folder `~/Development`, where `module2.nox` opens `module1.nox` and `module3.nox` opens `module2.nox`:

```bash
$ nox ~/Development/module1.nox ~/Development/module2.nox ~/Development/module3.nox
```

Executing this command will trigger the interpreter to run the code in each argument file, one after the other, in their order of apparition.

### Compiling Nox sources to Lua

While the REPL and interpreter can be useful to try out the language or to quickly prototype ideas, their performance is subpar due to the tree-walking nature of Nox's interpreter.
Therefore, the language also provides you with the option to compile your code to [Lua](https://www.lua.org/).

To do so, simply execute the `nox` command like you would to interpret a set of modules, but with the `-compile` flag set and with the `-output` option specified.

For example, to compile the three modules from the previous section's example to Lua and save the resulting code in a folder at location `~/Lua`, the following command can be used:

```bash
$ nox -compile ~/Development/module1.nox ~/Development/module2.nox ~/Development/module3.nox -output ~/Lua
```

Executing this command will produce three new files in the `~/Lua` folder called `module1.lua`, `module2.lua` and `module3.lua` and containing the Lua source code produced by the compiler for your code.

---

## A quick tour of the language

### Statements

A Nox program is simply a sequence of statements separated by semi-colons.
If the last statement in a sequence is an expression, the result obtained after evaluating it is returned as the sequence's return value.
If it isn't an expression, the value `()` of type `unit` (representing the absence of a meaningful value) is returned instead.

Below is an example of a sequence of statements defining two variables `x` and `y` and returning their sum:

```
let x = 1000;
let y = 337;
x + y
```

Statements in Nox can be [function declarations](#function-declarations), [variable definitions](#variable-definitions), [reference updates](#mutable-references), [while loops](#while-loops) or [expressions](#expressions). 

### Function declarations

Function declarations are identified by the `fun` keyword followed by a function name, a sequence of parameter names between parentheses and a [block](#blocks) of statements representing the function's body.

For example, the following statement declares a function `f` taking three variables `cond`, `a` and `b` as parameters and returning the sum of `a` and `b` if `cond` is `true`, or their difference if it is `false`:

```
fun f(cond, a, b) {
  if cond {
    a + b
  } else {
    a - b
  }
}
```

Although Nox programs look dynamically typed because they don't include any type annotations, they are actually statically typed thanks to type inference.
In the example above, the Nox compiler is able to determine that `cond` must be a `boolean` value because it used as the condition in a [conditional expression](#conditional-expressions), and that `a` and `b` must be `number`s because they are added or subtracted.
The type of the function is therefore inferred to be `(boolean, number, number) -> number`, indicating that it takes a `boolean` and two `number`s as arguments and returns another `number`.

The Nox compiler always infers the *most general* type possible for the expressions and statements in a program.
For example, when it encounters the function:

```
fun identity(x) {
  x
}
```

the compiler infers that `identity` is of type `('a) -> 'a`, where `'a` is a *generic type*, because any value could be used as the function's argument.

Because Nox is a higher-order functional programming language, its functions are first-class citizens.
This means that they can be stored in variables or passed as arguments to other functions.
They can also be *recursive* and *mutually recursive*. 
Mutually recursive functions must be declared directly one after the other, without any `;` between their definitions, as in the following example:

```
fun even(n) {
  if n == 0 {
    true
  } else if n > 0 {
    !odd(n - 1)
  } else {
    odd(n + 1)
  }
}

fun odd(n) {
  if n == 0 {
    false
  } else if n > 0 {
    !even(n - 1)
  } else {
    even(n + 1)
  }
};

even(12)
```

### Variable definitions

Variables can be defined with the `let` keyword followed by a name, the `=` symbol and an expression, as in the following example:

```
let x = 22 + 20
```

This statement creates a new variable called `x` in the current environment and associates it with the result of the evaluation of `22 + 20`, that is, `42`.
A variable's value can then be accessed by simply using its name in an expression.

Like in most other functional languages, variables in Nox are always *immutable*.
What it means is that the value of a variable cannot be changed after it has been defined.
The language however includes imperative features such as [mutable references](#mutable-references) to emulate the behaviour of mutable variables.

### Mutable references

Mutable reference cells can be created by simply prefixing any expression with the `&` operator.
A mutable reference is simply a memory cell containing a value that can be mutated during the execution of a program.
To access the value contained in a cell, the `@` operator must be used.

When a mutable reference cell is assigned to a variable, the value it contains can then be updated with a *reference update* statement.
A reference update statement consists in the name of a variable containing a mutable reference followed by the `<-` symbol and an expression indicating the new value to store in the cell.

For example, the following code defines a new variable `r` containing a mutable reference to a `string` (of type `&string`), prints its contents by accessing it with the `@` operator, updates its value with a new `string` and prints it again:

```
let r = &"Hello, World!";
print(@r);
r <- "Well, hello there!";
print(@r)
```

When updating a reference, the new value to be stored in it must have the same type as the value it previously contained.

The snippet above also illustrates our first use of the *built-in* function `print`, which takes a `string` as input and prints it to the standard output. 

### While loops

While loops in Nox are pretty standard.
They are written with the `while` keyword followed by a boolean expression denoting a condition and a block of statements to execute while the condition remains `true`.

For example, the following code uses a while loop to print the numbers from `0` to `5`:

```
let n = &0;
while @n < 6 {
  print(num2str(@n));
  n <- @n + 1
}
```

The snippet above also illustrates the use of the built-in `num2str` function, which converts a number to a string.
The call to this function is necessary: without it, we couldn't call `print` (recall that `print` takes a `string` as argument).
Nox also includes a built-in function to convert `boolean` values to strings: `bool2str`.

### Expressions

In addition to the standard [operations on booleans, numbers and strings](#basic-types-and-operations), expressions in Nox can be [blocks of statements](#blocks), [conditional expressions](#conditional-expressions), [record instanciations](#records) and [accesses to record fields](#records), [variants and match expressions](#variants), [lambdas](#lambda-expressions) and [module opens](#modules).

### Basic types and operations

Nox features four basic types of values:

- The `boolean` values `true` and `false`, which can be used as conditions in [while loops](#while-loops) and [conditional expressions](#conditional-expressions). The usual operations on booleans are available: logical or (`||`), logical and (`&&`) and negation (`!`);

- Values of type `number`, which can be integers (e.g. `42`) or floating point (e.g. `1.505`). Again, the usual operations on numbers are defined: comparisons (`<=`, `>=`, `<`, `>`), addition (`+`), subtraction (`-`), multiplication (`*`) and division (`/`);

- `string`s, denoted between `"`, which can be printed with the `print` function and concatenated with the `..` binary operator;

- `()`, of type `unit`, which denotes the absence of any meaningful value.

Values of any type can also be compared for equality with `==`, or for inequality with `!=`.
These operators implement *structural comparison*: two objects are equal if they represent the same values, irrelevant of their memory representations.

### Records

In addition to the four basic types, Nox also allows you to define and use more complex objects composed of values of different types, called records.
A record is simply a collection of named fields containing values of arbitrary types. 
They are similar to objects in Javascript or tables in Lua.

A record is defined by enclosing a sequence of assignments of field names to values between `[` and `]`, with field declarations separated by commas.
It is possible to copy the fields of an existing record in a new one and extend it with new values by adding a `|` followed by the record expression whose fields must be copied at the end of a record declaration.
A field in a record can be accessed by suffixing an expression denoting a record with a `.` followed by the field's name.

For example, the following snippet declares two records `r0` and `r1`, where `r1` extends `r0` with a new field `z`, and adds the values of fields `y` and `z` of records `r0` and `r1`, respectively:

```
let r0 = [x = 1, y = 3 b = true];
let r1 = [z = 10 | r0];
r0.y + r1.z
```

Records in Nox are *polymorphic*.
This means that, when a function accesses a field of one of its parameters, the compiler infers the most general record type possible for it.
For example, in the following code:

```
fun f(r) {
  r.x
}
```

the compiler infers that `r` must be of type `[x : 'a | 'b]`, meaning that any record containing a field `x` of any type can be passed to the function as arguments, even if it has other fields.

### Variants

Records allow you to group labeled values of different types together in the same object.
Variants, on the other hand, allow you to represent alternatives between labeled values of different types.
These variants can then be matched in special expressions to perform different actions depending on their labels.

A variant is declared by prefixing an expression of any type with a *label*. 
A label is any identifier prefixed with a `:`.
For example:

```
:B true
```

is a variant containing a value of type `boolean` and labelled with `:B`.

A match expression can be used to perform different actions depending on the value of a variant.
Match expressions are denoted by the `match` keyword, followed by some expression of variant type, and a series of *match arms* between `{` and `}`.
A match arm is represented by a label followed by a variable name, the `=>` symbol and an expression.
When a variant is being matched, the arm with the same label is selected, the value contained in the variant is bound to the arm's variable, and the expression after the `=>` is computed and returned.

In the following example, a variant containing a `number` and labelled with `:Some` is matched, the first arm is selected and the value `84.` is returned:

```
match :Some 42 {
  :Some n => 2 * n,
  :None _ => 0,
}
```

If the value being matched had been labelled with `:None`, the second arm would have been taken, and the value `0` returned.

### Blocks

A block expression is simply a sequence of statements enclosed between `{` and `}`.
When it is executed, the statements it contains are evaluated, and the value of the last one is returned.

For example, the following snippet returns `true`:

```
{
  let b0 = true;
  let b1 = false;
  b0 || b1
}
```

Blocks are a way to explicitly represent scopes for variables.
When a variable is defined inside of a block, it cannot be accessed outside of it.

### Conditional expressions

Conditionals are similar to `if` statements in other languages, with the difference that they are expressions.
This means that they return values and can be used in variable assignments.

For example, the following snippet assigns the value `2` to variable `a`:

```
let b0 = false;
let b1 = true;
let a = 
  if b0 {
    0
  } else if b0 && b1 {
    1
  } else {
    2
  }
```

### Lambda expressions

Lambda expressions allow you to define *anonymous functions*.
An anonymous function is simply a function without a name that can be used as an expression and called like any other function.

An anonymous functions is declared by enclosing its sequence of parameters between `<` and `>`, followed by a block expression for its body.

Below is an example of a program defining and immediatly calling a lambda expression with two arguments, returning the value `3`:

```
<x, y> {x + y}(1, 2)
```

### Modules

While it would be possible to define entire Nox programs in single files, the language gives you the option to split them into multiple modules to encourage composition and reusability.

In Nox, a module is simply a file containing a sequence of statements to be executed. 
Modules can be opened in a program with *open expressions*.
An open expression is denoted by the `open` keyword, followed by a string containing the module's name (the name of a module is simply the name of the file containing it, without the `.nox` extension).

When a module is opened, only the value of its last statement is returned.
This means that any function or variable declaration inside a module is private and cannot be accessed from the "outside".
Therefore, if you want to define functions that can be reused elsewhere, you need to store them (or "export" them) in a record that is returned as the last expression of the module.

For example, if we have the following module definition in a file called `vector2d.nox`:

```
fun add(v0, v1) {
  {x = v0.x + v1.x, y = v0.y + v1.y}
};

fun dot(v0, v1) {
  v0.x * v1.x + v0.y * v1.y
};

{add = add, dot = dot}
```

Then it is possible to load the function definitions it exports and use them as follows:

```
let vector2d = open "vector2d";

let v0 = {x = 1, y = 2};

let v1 = {x = 3, y = 10};

print(num2str(vector2d.add(v0, v1).x)); // Prints "4.".

print(num2str(vector2d.dot(v0, v1))) // Prints "23.".
```
