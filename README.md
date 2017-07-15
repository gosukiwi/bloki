# Bloki
Bloki is a functional static programming language which feels dynamic and looks blocky:

    // custom types are basically dicts
    [type person :name :age] // { name: nil, age: nil }
    // ^--- a block is anything between square brackets: `[...]'. All blocks eval to something.
    
    []  // the empty block is nil
    nil // equivalent statements

    // atoms evaluate to themselves
    1     // an int
    "foo" // a string
    1.23  // a float
    
    // let binding is immutable, cannot be changed.
    // special form
    [let array (:a-symbol 1 2 3 "string")] // binds to global context
    
    // when the first element of a block is a function, it gets called with the following items as parameters.
    [string/print "hi"]
    [string/index-at 1]
          
    // constructor
    [fn person/new [name age]
      [let p [set person :name name :age age]]
      p]

    // or simple
    [fn person/new [name age]
      [set person :name name :age age]]
    
    // whenever there is more than 1 parameter, it must be named
    [let p [person/new :name "Mike" :age 18]]
    // order doesn't matter
    [let p [person/new :age 18 :name "Mike"]]
    
    // function definition
    [fn person/grow [person]
      [let new-age [[person get: :age] + 1]
      [person set [:age new-age]]]]

    [person/grow p]
    
    // function with a single argument needs no name
    [fn foo [arg1]
      ...]
    [foo "hello"]
    
    // other functions must specify parameter name
    [fn foo [arg1 arg2]
      ...]

    [foo :arg1 "foo" :arg2 "bar"]
    // both valid
    [foo :arg2 "bar" :arg1 "foo"]

    // functions with missing arguments will return curried functions
    [fn foo [a b]
      [a + b]]
    [let add-one [foo 1]]
    [add-one 2] // => 3
      
    // binary functions
    [fn + [a b] [number/plus a b]]
    // when the first item in a block is an atom, it assumes a binary operation
    [1 + 2]

    // we can define a >> operator which does chaining
    [[foo :arg1 "foo" arg2: "bar"] >> [string/to_s]]
    // [ atom >> function ]

    // >> calls the second function using the result of the LHS as the LAST argument of the RHS
    [fn >> [a b]
      [b a]]
      
    // string interpolation
    [let name "Mike"]
    [let greet "Hello, #{name}!"]
    
    // conditionals
    [if t
      "True"
      "False"]
      
    [match age with
      [[< 18] "Under 18"]
      [18     "Old enough to drink in some places"]]
      [[> 50] [do-something-with age]]]
    // match takes a value and a bunch of blocks. 
    // it tests each block with the first element of it.
    // if the first element is a function, it calls that function with the parameter, for example, `[< 10]` returns a function which is true when the first parameter is smaller than 10, because of currying
    // if the first element is a value, it compares it using ==

    [fn say-type [value]
      [match foo with
        [number "It's a number"]
        [string "It's a string"]]]
    // the compiler knows the type of foo, it knows what match expects
    [say-type "hi"]                      // => "It's a string"
    [say-type [person/new :name "Mike"]] // => Compiler error

    [fn say-type [value]
      [match foo with
        [number "It's a number"]
        [string "It's a string"]
        [t      "I don't even know"]]]
    [say-type [person/new :name "Mike"]] // => "I don't even know"

    // comparison
    // comparison is semantic, each native atom implements the `==' function.
    [1 = 1] // => true
    [1 = 2] // => false
    // same as doing
    [number/eq? a: 1 b: 1]
    [number/eq? a: 1 b: 2]
    // the = function does a big match, basically
    
    // implement equality
    // override the function. because changing things is explicit in bloki, we need to use `fn-override` instead of `fn`
    [fn-override = [a b]
      [match a with
        [[atom/is? a :person] [a person/= b]]
        // fallback to old function defintion
        [t                    [a = b]]]]
    
    // loops
    // TODO: Copy some of Lisps loops?
    
    [times 10 [print "hi"]]
    
    [let a (1 2 3)]
    [each [lambda [it]
      [print it]] a]
      
    // a shorter way to do lambdas
    [each #[it| [print it]] a]

    // an even shorter way
    [each #[print it]]
    
    // chaining
    a >> [each #[print it]]
    a >> [map #[it > 1]] >> [each #[print it]]
    
    // while-do
    [var x 0]
    [while [x < 10] 
      [set x [x + 1]]]

    [while t
      [print "hello!"]
      [break]]

    [macro defp [name body]
      [fn ,name [] [
        #[input| ,body]
      ]]
    [defp fooparser "Input is: #{input}"]
    // will be compiled to
    // [fn fooparser [] [
    //   #[input| "Input is #{input}"]
    // ]

The core concept of Bloki are blocks of code. A program is just a bunch of blocks executed in order. Bloki aims to be a minimal, expressive and powerful language.
The syntax is very simple, and extensible via simple macros. It doesn't get in the way, it doesn't need to be used in a functional pure way.

It has functional goodies like functions as first class citizens, variables default to immutable, immutable data structures, and built-in currying. It also has
an script feel so it's easy to think out algorithms any way you like.

## Good Practices
Bloki enforces some good practices, like named parameters and complete functions.
    
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

    (asdf:test-system :my-app)
    (asdf:test-system :my-app-test)
    
    ;; Same as 'asdf:test-system' except it returns T or NIL as the result of tests.
    (prove:run :my-app-test)