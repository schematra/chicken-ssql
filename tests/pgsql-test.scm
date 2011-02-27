(load-relative "../ssql-pgsql")
(import ssql)
(import ssql-pgsql)
(use test postgresql foops)

(define *test-pgsql-translator* 
  (derive-object (*pgsql-translator* self super)
                 ((escape-string string)
                  (super (ssql-connection) string))))

(register-sql-engine! (lambda (x) (eq? x #t)) *test-pgsql-translator*)

(test-begin "selects")
(test "Simple query"
  "SELECT actors.firstname, actors.lastname FROM actors"
  (ssql->sql #t `(select (columns actors.firstname actors.lastname)
                   (from actors))))
(test-end "selects")