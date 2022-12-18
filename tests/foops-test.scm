(import (chicken load) foops srfi-1 test)

(test-group "basic functionality"
  (define foo (make-object () ((bar) 'baz)))
  (test 'baz (foo 'bar))

  (define widget
    (make-object (self)
                 ((frob) 'foo)
                 ((echo s) s)
                 ((send m) (self m))))

  (test 'foo (widget 'frob))
  (test "here" (widget 'echo "here"))
  (test 'foo (widget 'send 'frob))
  (test-error (widget 'err)))

(test-group "inheritance"
  (define dinosaur 
    (make-object (self)
                 ((sound) 'rawr)
                 ((talk) (self 'sound))))

  (define t-rex
    (derive-object (dinosaur self super)
                   ((sound) 'hah)
                   ((talk-original) (super 'sound))))

  (test 'rawr (dinosaur 'talk))
  (test 'hah (t-rex 'talk))
  (test 'rawr (t-rex 'talk-original)))


(test-group "pattern matching"
  (define vehicle 
    (make-object (self)
                 ((drive) 'wroom)
                 ((drive distance) 
                  (map (lambda (i) (self 'drive))
                       (iota distance)))
                 ((drive distance speed)
                  (cons speed (self 'drive distance)))))

  (test 'wroom (vehicle 'drive))
  (test '(wroom wroom) (vehicle 'drive 2))
  (test '(fast wroom wroom wroom) (vehicle 'drive 3 'fast)))