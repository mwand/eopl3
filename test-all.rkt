#lang racket

;;; script to test all eopl interpreters

(require (prefix-in chapter3/let-lang/top.scm
                    "chapter3/let-lang/top.scm"))
(chapter3/let-lang/top.scmtest-all)
"chapter3/let-lang/top.scm"


(require (prefix-in chapter3/letrec-lang/top.scm
                    "chapter3/letrec-lang/top.scm"))
(chapter3/letrec-lang/top.scmtest-all)
"chapter3/letrec-lang/top.scm"


(require (prefix-in chapter3/lexaddr-lang/top.scm
                    "chapter3/lexaddr-lang/top.scm"))
(chapter3/lexaddr-lang/top.scmtest-all)
"chapter3/lexaddr-lang/top.scm"


(require (prefix-in chapter3/proc-lang/ds-rep/top.scm
                    "chapter3/proc-lang/ds-rep/top.scm"))
(chapter3/proc-lang/ds-rep/top.scmtest-all)
"chapter3/proc-lang/ds-rep/top.scm"


(require (prefix-in chapter3/proc-lang/proc-rep/top.scm
                    "chapter3/proc-lang/proc-rep/top.scm"))
(chapter3/proc-lang/proc-rep/top.scmtest-all)
"chapter3/proc-lang/proc-rep/top.scm"


(require (prefix-in chapter4/call-by-need/top.scm
                    "chapter4/call-by-need/top.scm"))
(chapter4/call-by-need/top.scmtest-all)
"chapter4/call-by-need/top.scm"


(require (prefix-in chapter4/call-by-reference/top.scm
                    "chapter4/call-by-reference/top.scm"))
(chapter4/call-by-reference/top.scmtest-all)
"chapter4/call-by-reference/top.scm"


(require (prefix-in chapter4/explicit-refs/top.scm
                    "chapter4/explicit-refs/top.scm"))
(chapter4/explicit-refs/top.scmtest-all)
"chapter4/explicit-refs/top.scm"


(require (prefix-in chapter4/implicit-refs/top.scm
                    "chapter4/implicit-refs/top.scm"))
(chapter4/implicit-refs/top.scmtest-all)
"chapter4/implicit-refs/top.scm"


(require (prefix-in chapter4/mutable-pairs/top.scm
                    "chapter4/mutable-pairs/top.scm"))
(chapter4/mutable-pairs/top.scmtest-all)
"chapter4/mutable-pairs/top.scm"


(require (prefix-in chapter5/exceptions/top.scm
                    "chapter5/exceptions/top.scm"))
(chapter5/exceptions/top.scmtest-all)
"chapter5/exceptions/top.scm"


;(require (prefix-in chapter5/letrec-lang/top.scm
;                    "chapter5/letrec-lang/top.scm"))
;(chapter5/letrec-lang/top.scmtest-all)
;"chapter5/letrec-lang/top.scm"


(require (prefix-in chapter5/thread-lang/top.scm
                    "chapter5/thread-lang/top.scm"))
(chapter5/thread-lang/top.scmtest-all)
"chapter5/thread-lang/top.scm"


(require (prefix-in chapter6/cps-lang/top.scm
                    "chapter6/cps-lang/top.scm"))
(chapter6/cps-lang/top.scmtest-all)
"chapter6/cps-lang/top.scm"


(require (prefix-in chapter6/cps-side-effects-lang/top.scm
                    "chapter6/cps-side-effects-lang/top.scm"))
(chapter6/cps-side-effects-lang/top.scmtest-all)
"chapter6/cps-side-effects-lang/top.scm"


(require (prefix-in chapter7/checked/top.scm
                    "chapter7/checked/top.scm"))
(chapter7/checked/top.scmtest-all)
"chapter7/checked/top.scm"


(require (prefix-in chapter7/inferred/top.scm
                    "chapter7/inferred/top.scm"))
(chapter7/inferred/top.scmtest-all)
"chapter7/inferred/top.scm"


(require (prefix-in chapter8/abstract-types-lang/top.scm
                    "chapter8/abstract-types-lang/top.scm"))
(chapter8/abstract-types-lang/top.scmtest-all)
"chapter8/abstract-types-lang/top.scm"


(require (prefix-in chapter8/full-system/top.scm
                    "chapter8/full-system/top.scm"))
(chapter8/full-system/top.scmtest-all)
"chapter8/full-system/top.scm"


(require (prefix-in chapter8/simplemodules/top.scm
                    "chapter8/simplemodules/top.scm"))
(chapter8/simplemodules/top.scmtest-all)
"chapter8/simplemodules/top.scm"


(require (prefix-in chapter9/classes/top.scm
                    "chapter9/classes/top.scm"))
(chapter9/classes/top.scmtest-all)
"chapter9/classes/top.scm"


(require (prefix-in chapter9/typed-oo/top.scm
                    "chapter9/typed-oo/top.scm"))
(chapter9/typed-oo/top.scmtest-all)
"chapter9/typed-oo/top.scm"

