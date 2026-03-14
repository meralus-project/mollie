## Before You Begin

This documentation does NOT describe or cover specific language implementations in detail; it merely touches on some of them.

It is primarily intended to provide an introduction to the language’s structure for those who are just beginning to learn it and/or are unfamiliar with other programming languages.

For more detailed information about its implementation, see [`docs`](../docs/README.md).

## Navigation

- [What is Mollie?](#what-is-mollie)
- [Syntax](#syntax)
  - [Expressions](#expressions)
    - [Primitive Values](#primitive-values)
    - [If-else and Loops](#if-else-and-loops)
    - ...
  - [Statements](#statements)
    - [Variables](#variables)
    - ...
- Type System
  - Primitives
  - Structures
  - Enumerations
  - Views
  - Traits
  - Functions
- ...

## What is Mollie?

Mollie is a scripting language with a strict type system, much like the Rust programming language.

However, unlike Rust, Mollie has a garbage collector. This is largely because implementing a full-fledged ownership and lifetime system in a language designed to be relatively easy to learn and use would be somewhat impractical.

Also, one of Mollie’s features is a separate type designed primarily for describing user interfaces in a Qt-like style. It’s called `view`. We’ll talk about it in more detail a little later. For now, let’s take a look at...

## Syntax

It’s hard to call Mollie an "ideal language" with a well-thought-out syntax that has no unnecessary elements. If you’re already familiar with languages like JavaScript, C#, Go, and Rust, Mollie’s syntax will seem like an attempt to borrow a little bit from each of them. And that’s exactly what it is. Perhaps the language’s creators could have been a bit more creative, but let’s get to the point.

Mollie consists of two parts: expressions and statements. In fact, most of the syntax consists of expressions. So let’s start with those.

### Expressions

The core functionality of the language, allowing you to do everything you’re used to. Function calls, arrays, loops, code blocks. All of this is an expression! I’ll also note that if an expression doesn’t end with a code block or isn’t a value returned from a function, it must end with a semicolon.

#### Primitive Values

As in most languages, it all starts with numbers. Well, or strings! Essentially, there’s nothing special here:

- `1`: Integers are written as usual.
- `1.0`: Floating-point numbers use a `.` to separate the integer and fractional parts.
- `"Hello, World!"`: Strings are enclosed in double quotes.
- `true`, `false`: Boolean values use `true` and `false`.

In the future, there may be a character as a separate type, whose value will be created via a character enclosed in single quotes, but this is not currently the case.

It’s also worth noting that numbers can contain an optional suffix indicating either their specific type (`1u64`) or a suffix function (`1.0px`).

### If-else and Loops

They are combined into a single section because, essentially, both affect the "flow" of the resulting code. Let’s start with if-else, then move on to loops, and finish with a couple of examples.

If you’re mostly familiar with C-like languages such as JavaScript or C itself, you may have noticed that in Mollie, the if condition doesn’t need to be enclosed in parentheses. This is an intentional and final decision in the language’s design.

What about loops? Here, you have several options.

- The familiar `while` loop, which executes code as long as the condition is true.
- `for in` for iterating over array elements (and more).
- An infinite loop using `loop`.

In each of these, you can use `break` to force the loop to terminate and `continue` to skip the next iteration and move on to the next one.

Now, let’s look at some examples!

```rust
if 4 > 4 {
  println("How is that possible?! :fearful:");
}
```

```rust
while true {
  println("Why not use a loop?");
}
```

```rust
for num in [1, 2, 3] {
  if num == 2 {
    continue; // I don't like this number, let's skip it
  }

  println(num);
}
```

### Statements

These are just as important as expressions. They are what you use to declare everything: variables, functions, and types. A semicolon is required only if it is a variable declaration or an expression that does not end a code block and is not a return value at the end of a function.

#### Variables

They are declared using `const` or `let`. The difference is that variables declared with `const` cannot be changed. The value of a variable can be *almost* anything. That is, you can use if-else statements, code blocks, and in theory, even loops! Although in practice, you’re unlikely to do the latter. Also, every variable declaration requires a semicolon at the end. Here are a couple of examples:

```rust
const protected = "I'm protected by the language rules!";
```


```rust
let dynamic = true;

dynamic = false;
```

```rust
const should_be_true = if true == true {
  "Reality is stable!"
} else {
  "It seems we're in danger..."
};
```
