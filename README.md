This is all the code from the book Essentials of Programming
Languages, 3rd edition, by Friedman and Wand.

The code dates from 2009.  It was last thoroughly tested in PLT Scheme
versions 372 and 4.2, and is believed to run unchanged in Racket 5.1.3
(August 2011).

For Racket version 5.3.5 (June 2013), replace all instances of
`all-defined` by `all-defined-out`.

If someone would like to do this project, send me a pull request.

If you want to make it more consistent with current Racket code,
remove the wrapping that says 
```
(module name (lib "eopl.ss" "eopl") ....  )
```
around each file, and write `#lang eopl` at the top of the file
instead. If the module says 
```
(module name mzscheme ....  )
```

use `#lang mzscheme` instead (NOT `#lang racket` -- the syntax for
require/provide is slightly different).  (Or you can adapt it to use `#lang racket`).

If you are feeling more adventurous, you can try to adapt the code
base to use the rackunit testing framework instead of the kludgy one I
threw together for the book. 

Another useful project would be a testing script so I can easily test the
code against new versions of Racket as they appear.

Again, if anybody would like to undertake any of these, feel free, and
send me a pull request.  --Mitch

