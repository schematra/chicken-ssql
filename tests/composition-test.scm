(import ssql test)

(test-group "composition"
  (test "simple"
    '(select (columns firstname lastname) (from artists) (where (= firstname "Frank")))
    (ssql-compose #f
                  '(select (columns firstname lastname) (from artists))
                  '((where (= firstname "Frank")))))

  (test "merge"
    '(select (columns firstname lastname age) (from artists) (order firstname lastname))
    (ssql-compose #f '(select (columns firstname lastname) (order firstname))
                  '((columns age) (from artists) (order lastname)))))