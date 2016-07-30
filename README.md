# SECURE-READ
The goal of this project is to create a reader that is able to read input from untrusted sources, such as internet sockets.

As of now, this includes variant of READ secure against internbombing, excessive input and macro characters.

### Rough definitions
* **Internbombing** - the behaviour of READ function, which automatically interns every unknown symbol it doesn't know. This can lead to namespace pollution and blowing the heap with input like `(symbol1 symbol2 ... symbol9999999999)`.
* **Excessive input** - causing an out of memory error simply by feeding the READ function an invalid S-expression that does not terminate.
* **Macro characters** - functioning of character macros within Lisp syntax, both standard and implementation-defined. They create simple dangers such as `#.(let (memleak) (loop (setf memleak (cons memleak memleak))))` along with more subtle ones that are not listed here.

### Function SAFE-READ
_`&optional (stream *standard-input*)` **→** `s-expression error-status`_
  * `S-EXPRESSION` - the read S-expression or NIL if reading was impossible.
  * `ERROR-STATUS` - one of `:INCOMPLETE-INPUT :MALFORMED-INPUT :INPUT-SIZE-EXCEEDED` or NIL in case of success.

### Variable *MAX-INPUT-SIZE*
  * Input will not be read beyond this size; SAFE-READ will return `:INPUT-SIZE-EXCEEDED` as its second value.
  * Defaults to 128 kB.

### Details
  * Only lists are accepted as input. Atoms produce `:MALFORMED-INPUT`.
  * All macro characters other than `#\(` `#\)` `#\"` produce `:MALFORMED-INPUT`.
  * Package-qualified names (including keywords) produce `:MALFORMED-INPUT`.
  * All symbols read are non-interned.
  * Read always stops on EOF and newlines.
  * Reading from each stream is buffered, meaning that subsequent calls to SAFE-READ can produce a valid S-expression if it spans over multiple lines. An example is a list containing a string containing a newline.

### TODO
  * `make it possible to signal conditions instead of relying on the second return value.`
