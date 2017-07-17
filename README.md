# Bloki

Bloki is a functional tiny programming language with safe defaults and blocky look:

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
      [if name
        "Name is #{name}"
        nil]] // ERROR, must return string or nil, but not both

The fix is easy

    [fn foo [name]
      [if name
        "Name is #{name}"
        "Anonymous"]]
        
In Bloki, `nil` is an empty block of code, so it's the same as writing `[]`, nevertheless it's
reccommended to just use the `nil` keyword.

A Bloki program is just a bunch of blocks executed in order. The syntax is very simple, and extensible via 
simple macros. It doesn't get in the way.

Bloki doesn't get in the way, it's a functional language with goodies like
functions as first class citizens, immutable "variables" by default, immutable
data structures, and built-in currying. But it also has an script feel so it's easy
to think algorithms any way you like.

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
