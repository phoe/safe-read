# SECURE-READ
A variant of READ secure against internbombing, excessive input and macro characters.

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
