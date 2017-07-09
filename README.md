Bloki lang
============================================

Bloki is a functional dynamic programming language which looks blocky. It looks like this:

    // atoms are basically dicts
    [atom person :name :age] // { name: nil, age: nil }
    // ^--- a block is anything between square brackets: `[...]'. All blocks eval to something.
    
    []  // the empty block is nil
    nil // equivalent statements
    
    // let binding, immutable, cannot be changed.
    // special form
    [let array (:a-symbol 1 2 3 "string")] // binds to global context
    
    // when the first element of a block is a function, it gets called with the following items as parameters.
    [print "hi"]
    [string/index-at 1]
    
    // default constructor
    [let p [new person]] // syntax sugar for as [person dup]
    
    // defining `new' using a macro.
    // the compiler will read this, and replace all `[new _]' blocks with the body, of this block "as a string"
    // and the string will be evaled instead.
    [macro new [name]
      "[dup name]"]
    
    // constructor
    [fn person/new [name age]
      [let p [set person :name name :age age]]
      p]
    
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
    [fn + [a b] [a plus b]]
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
    
    // comparison
    // comparison is semantic, each native atom implements the `==' function.
    [1 = 1] // => true
    [1 = 2] // => false
    
    [= a: 1 b: 2] // => false, using the other way of calling functions
    
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
    [each #[print it] ]
    
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
