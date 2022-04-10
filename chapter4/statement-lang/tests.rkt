#lang racket

(provide test-list)
;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(define test-list
  '(

    (run "var x,y; {x = 3; y = 4; print +(x,y)}" )
    (run "var x,y,z; {x = 3;
                    y = 4;
                    z = 0;
                    while not(zero?(x))
                      {z = +(z,y); x = -(x,1)};
print z}")
    ;; 12
    (run " var x; {x = 3;
                 print x;
                 var x; {x = 4; print x};
                 print x}")
    ;; 3
    ;; 4
    ;; 3
    (run "var f,x; {f = proc(x,y) *(x,y);
                  x = 3;
                  print (f 4 x)}")
    ;; 12

    ))
