# Bloki
Bloki is a tiny functional programming language with safe defaults and blocky look:

    [fn sum [x y]
      [x + y]]
    [let add-one [sum 1]]
    [add-one 3] // => 4

The core concept of Bloki are blocks of code. The simplest block of code is just what's called an `atom`.

    1
    "hello, world!"
    1.5

More complex blocks are wrapped with square brackets:

    [string/blank? "Hello"]          // => Run `string/blank?` function with input "Hello"
    [1 + 2]                          // => Run `+` function with `1` and `2`
    [make-user :name "Mike" :age 18] // => Call `make-user` with two named parameters
    
Bloki wants solid conventions. Whenever a function has more than one parameter, they must be named
parameters. When the function takes only two parameters, it can be called in the _binary form_, which is
`arg1 function-name arg2`, eg: `2 + 2`.

When a function takes a single parameter, there's no need for naming it: `[print "Hello!"]`.

In Bloki, functions must be total, that means, they must always return the same value:

    [fn foo [name]
      [if [[str/length name] > 3]
        "Name is #{name}"
        18]] // ERROR, must return string or number, but not both

The fix is easy

    [fn foo [name]
      [if [[str/length name] > 3]
        "Name is #{name}"
        "Your name is too short... I'll call you Mike."]]
        
If you really don't want another string, you can use `maybe`:

    [fn foo [name]
      [if [[str/length name] > 3]
        [maybe "Name is {name}"]
        [maybe-not]]]

    [resolve [foo "Mike"]
      [print name]         // when it has a value
      [do-something-else]] // when it doesn't
        
A Bloki program is just a bunch of blocks executed in order. The syntax is very simple, and extensible via 
simple macros. It doesn't get in the way.

Bloki doesn't get in the way, it's a functional language with goodies like
functions as first class citizens, immutable "variables" by default, immutable
data structures, and built-in currying. But it also has an script feel so it's easy
to think algorithms any way you like.

# Goals
Bloki aims to provide:

* Garbage collection
* An interactive development cycle though a decent REPL - Like Lisp
* Dynamic feel, try to be as safe as possible with little info (eg, functions return always the same type, useless variables, etc) - Like F#
* Provide and force built-in syntactical analysis (don't allow messy code) - Like Rubocop
* There is no nil, only a `maybe` construct: `[maybe "I'm a string"]` - Like OCaml
* Be great for web development - Like Ruby
* Have a great ecosystem (this is the hardest, needs lots of stuff) - Like Ruby
* Exceptions only make things worse, allow functions to return multiple values. Together with `maybe` it's enough - Like Go
* There are ways to throw errors though, but they are terminal errors and very rare - Like Go

Bloki does not yet aim to:

* be fast
* run everywhere
* be stable

# Developers
Bloki is implemented using Common Lisp. It compiles bloki code down to Lisp, it can then eval it or emit machine code.

In case you haven't, set up ASDF:

    (setf asdf:*central-registry*
       ;; Default directories, usually just the ``current directory''
      '(*default-pathname-defaults*
    
        ;; Additional places where ASDF can find
        ;; system definition files
        #p"/home/foo/lisp/systems/"
        #p"/usr/share/common-lisp/systems/"))

Then just link `lib/bloki.asd`

    $ git clone bloki /home/foo/code/bloki
    $ cd /home/foo/lisp/systems
    $ ln -s /home/foo/code/bloki/lib/bloki.asd

## Running tests

    (ql:quickload "bloki-test")
    (prove:run :bloki-test :reporter :dot)
    
You'll need to set up [prove](https://github.com/fukamachi/prove).

## TODO
It's still not sure whether how Bloki code should run. It could run in a Lisp-powered VM, and be compiled as bytecode. Lisp can generate cross-platform self-contained executables.
That might take too much effort though, it could be compiled down to Lisp code and then use Lisp to compile that code, generating and executable and thus compiling to native code though Lisp.
The downside is that debugging would be harder, as there would be no way to easily make a stepping debugger. Bloki would be much faster though.
