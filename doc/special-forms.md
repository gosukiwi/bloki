# Special Forms
The only blocks which break the rules are what's called _Special Forms_. The concept is
borrowed from the Lisp world. Bloki provides a basic macro system which is not capable of
generating all the special forms, so some are provided to make life easier.

You can think of special forms as something magical the language gives you, the implementation
is native. Like most languages native functions and constructs.

Special forms all start with a keyword, which is the name of the form. The name
is followed by whatever the form defines. For example, a `let` special form is
followed by an identifier representing the name of the variable and a block,
which represents the value: `[let x 1]` or `[let four [2 * 2]]`.

## Atoms
Atoms are expression which represent themselves, you can think of it as the base
building blocks for all blocks, something which is not made of smaller things.

Atoms are: strings, numbers, symbols and identifiers.

    "I'm a string"
    123
    :a-symbol
    an-identifier

## Variables
While they are called _variables_ just to make things easier, they do not normally
change their value, so they might as well be called _binding_, as they bind a name
to a value.

Variables can be mutated though, but you have to do that explicitly.

    [let pi 3.1416]
    pi // => 3.1416
    [set pi 0] // => error, can't redefine
    
    [var age 18]
    [set age 22]
    age // => 22

## Conditionals
The basic conditional form is `if`:

    [if t
      "True"
      "False"] // => "True"
      
There's syntactic sugar for testing conditions:

    [when t [...]]
    [unless t [...]]
    
## Calling functions
Functions can be called in 3 ways:

    [print "Hello, World!"]

When the function takes a single argument, we just pass it. The function name goes first, then the argument.

When there is more than one argument, we must use named parameters:

    [make-person :name "Federico" :age 27 :likes "cats"]

Here `make-person` is the name of a function. A symbol is used to represent the name of the argument, followed by it's value.

The final way of calling functions only works for functions with 2 arguments, and it's called _binary function call_:

    [2 + 2]
    
In that case, we put the name of the function - `+` - in the middle of the block, surrounded by it's arguments, just like any
binary operation in math.

You can nest function calls, that's the whole idea of bloki!

    [[foo 1] + [3 * 1]]

## Defining functions

You can of course define your own functions, the syntax is simple:

    [fn add [x y]
      [x + y]]

We defined the add function, which takes to arguments, x and y. We
can now call it: `[add x: 1 y: 1]`.

As you might suspect, there's an easier way to do this:

    [1 add 1]

You can make your own binary functions!

## Chaining functions
The `>>` special form allows you to pass something as the last argument of a function. For example:

    1 >> [add x: 2] // => 3

This is particularly useful for chaining stuff:

    (1 2 3) >> [map #[it * 2]] >> [each #[print it]]

## Lambdas
Lambdas are functions without a name, you can define them with `lambda`:

    [lambda [x]
      [do-something-with x]]

There is of course syntax sugar:

    #[x|
      [do-something-with x]]
      
If the function takes a single argument, you can leave it and use the default
argument: `it`.

    #[do-something-with it]

## Arrays
Arrays are defined using parentheses, and can contain things from different types:

    (1 2 "foo" :bar)

## Types

### Inheritance

## Macros

