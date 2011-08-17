(module ssql

(ssql->sql ssql-connection scope-table find-tables ssql-compose
 register-sql-engine! define-operators *ansi-translator*)

(import chicken scheme)
(use matchable data-structures extras srfi-1 srfi-13 foops)
(import-for-syntax chicken)
(begin-for-syntax
 (use srfi-1 srfi-13))

(define (before? x y lst)
  (let loop ((lst lst))
    (or (null? lst)
        (eq? x (car lst))
        (and (not (eq? y (car lst)))
             (loop (cdr lst))))))

(define-syntax define-operators
  (ir-macro-transformer
   (lambda (x i c)
     (let ((engine (second x)))
       `(set! ,engine
              (derive-object (,engine self)
                             ,@(map (lambda (op)
                                      (let ((ssql-op (first op))
                                            (type (second op)))

                                        (unless (memq (i type) '(infix infix* suffix prefix function))
                                          (error "unknown operator syntax type" type))

                                        (let-optionals (cddr op)
                                            ((sql-op (string-upcase (->string (strip-syntax ssql-op))))
                                             (separator #f))
                                          `((,(i ssql-op) operands)
                                            (self 'operator->sql ',type ,sql-op ,separator operands)))))
                                    (cddr x))))))))

(define *ansi-translator*
  (make-object (self)
               ((type->sql-converters) 
                `((,null? . null->sql)
                  (,pair? . pair->sql)
                  (,symbol? . source->sql)
                  (,string? . string->sql)
                  (,number? . number->sql)
                  (,vector? . vector->sql)))

               ((clauses-order) '(columns from where order having union))

               ((escape-string string)
                (string-translate* string '(("'" . "''"))))

               ((null->sql null) "")

               ((pair->sql pair)
                (self (car pair) (cdr pair)))
               
               ((values vals)
                (sprintf "(~A)"
                         (string-intersperse
                          (map (lambda (s)
                                 (self 'ssql->sql s #t))
                               vals)
                          ", ")))

               ((vector->sql vec)
                (self 'values (vector->list vec)))

               ((string->sql string)
                (string-append "'" (self 'escape-string string) "'"))

               ((number->sql number)
                (->string number))

               ((source->sql source)
                (symbol->string source))

               ((columns ssql)
                (string-intersperse (map (lambda (s)
                                           (self 'ssql->sql s))
                                         ssql)
                                    ", "))

               ((col ssql)
                (string-intersperse (map (lambda (colname)
                                           (self 'ssql->sql 
                                                 (string->symbol (sprintf "~A.~A" 
                                                                          (car ssql)
                                                                          (self 'ssql->sql colname)))))
                                         (cdr ssql))
                                    ", "))

               ((join ssql)
                (match ssql
                  ((type first rest ...) (sprintf "(~A ~A JOIN ~A)"
                                                  (self 'ssql->sql first)
                                                  (string-upcase (symbol->string type))
                                                  (string-join (map (lambda (x) (self 'ssql->sql x)) rest))))))

               ((set values)
                (string-append "SET "
                               (string-intersperse (map (lambda (val)
                                                          (sprintf "~A = ~A" 
                                                                   (first val)
                                                                   (self 'ssql->sql (second val))))
                                                        values)
                                                   ", ")))

               ((insert into values)
                (sprintf "INSERT INTO ~A VALUES ~A"
                         into
                         (string-intersperse (map (lambda (val) 
                                                    (self 'ssql->sql val))
                                                  values) ", ")))

               ((insert (('into table) ('columns columns ...) values ...))
                (self 'insert
                      (sprintf "~A (~A)" 
                               table
                               (string-intersperse (map symbol->string columns) ", "))
                      values))

               ((insert (('into table) values ...))
                (self 'insert table values))

               ((operator->sql type operator separator operands)
                (case type
                  ((infix)
                   (sprintf "(~A)" (self 'operator->sql 'infix* operator separator operands)))

                  ((infix*)
                   (string-intersperse
                    (map (lambda (operand) 
                           (self 'ssql->sql operand))
                         operands)
                    (string-append " " operator " ")))

                  ((function)
                   (sprintf "~A(~A)"
                            operator
                            (string-intersperse
                             (map (lambda (operand) 
                                    (self 'ssql->sql operand))
                                  operands)
                             (or separator ", "))))
                  ((suffix prefix)
                   (let ((operator (if (eq? type 'prefix)
                                       (string-append operator " ")
                                       (string-append " " operator))))
                     (string-join
                      (list
                       (string-intersperse
                        (map (lambda (operand)
                               (self 'ssql->sql operand))
                             operands)
                        (or separator " ")))
                      operator
                      type)))
                  (else (error "unknown operator syntax type" type))))

               ((ssql->sql ssql parenthesize?)
                (let ((handler (alist-ref (list ssql) (self 'type->sql-converters) apply)))
                  (if handler
                      (if (and parenthesize? (pair? ssql))
                          (sprintf "(~A)" (self handler ssql))
                          (self handler ssql))
                      (error "unknown datatype" ssql))))

               ((ssql->sql ssql)
                (self 'ssql->sql ssql #f))

               ((insert-clause clause ssql)
                (let ((order (self 'clauses-order)))
                  (let loop ((ssql ssql))
                    (cond ((null? ssql) (list clause))
                          ((before? (car clause) (caar ssql) order)
                           (cons clause ssql))
                          (else (cons (car ssql) (loop (cdr ssql))))))))

               ((merge-clauses target-clause clause)
                (append target-clause (cdr clause)))))

(define-operators *ansi-translator*
  (select prefix)
  (update prefix)
  (from prefix "FROM" ", ")
  (where prefix)
  (order prefix "ORDER BY" ", ")
  (having prefix)
  (union infix)
  (as infix*)
  (asc suffix)
  (desc suffix)
  (on prefix)

  (and infix)
  (or infix)
  (not prefix)

  (min function)
  (max function)
  (avg function)
  (sum function)
  (count function)

  (distinct prefix)
  (all prefix)

  (upper function)
  (lower function)
  (string-append infix "||")
  (= infix)
  (like infix)
  (in infix)
  (escape infix)
  (< infix)
  (> infix)
  (<= infix)
  (>= infix)
  (<> infix)
  (!= infix "<>")
  (null? suffix "IS NULL"))

(define *sql-engines* `((,(lambda (obj) (eq? obj #f)) . ,*ansi-translator*)))

(define ssql-connection (make-parameter #f))

(define (register-sql-engine! predicate translator)
  (set! *sql-engines* (alist-cons predicate translator *sql-engines*)))

(define (get-sql-engine connection)
  (alist-ref (list connection) *sql-engines* apply))

(define (call-with-sql-engine connection proc)
  (let ((engine (get-sql-engine connection)))
    (if engine
	(parameterize ((ssql-connection connection))
          (proc engine))
	(error (sprintf "No engine found for connection object ~A" connection)))))

(define (ssql->sql connection ssql)
  (call-with-sql-engine connection
    (lambda (engine)
      (engine 'ssql->sql ssql))))

(define (escape connection string)
  ((get-sql-engine connection) 'escape string))

(define (colref? x)
  (and (symbol? x) (string-any #\. (symbol->string x))))

(define (rewrite-tables ssql renamed)
  (let loop ((ssql ssql))
    (match ssql
      (('col alias cols ...) `(col ,(alist-ref alias renamed eq? alias) ,@cols))
      (('as table alias) `(as ,table ,(alist-ref alias renamed eq? alias)))
      ((? colref? col)
       (let* ((refs (string-split (symbol->string col) "."))
              (col (string->symbol (car refs))))
         (string->symbol (string-join (cons
                                       (symbol->string (alist-ref col renamed eq? col))
                                       (cdr refs))
                                      "."))))
      ((operator operands ...)
       `(,operator ,@(map (cut loop <>) operands)))
      (other other))))

(define (scope-table table scope ssql)
  (let loop ((ssql ssql))
   (match ssql
	  ((not (? pair?)) ssql)
	  ((select ('columns tables ...)
		   ('from from-specs ...)
		   ('where conditions ...)
		   more ...)
	   (let ((aliases (filter (lambda (x) (eq? (car x) table)) (find-tables (cons 'from from-specs)))))
	     `(select (columns ,@(map loop tables))
		      (from ,@(map loop from-specs))
		      (where (and ,@(map (lambda (alias) (rewrite-tables scope `((,table . ,(cdr alias))))) aliases)
				  ,(map loop conditions)))
		      ,@(map loop more))))
	  ((select ('columns tables ...)
		   ('from from-specs ...)
		   more ...)
	   (=> fail)
	   (let ((aliases (filter (lambda (x) (eq? (car x) table)) (find-tables (cons 'from from-specs)))))
	     (if (null? aliases)
		 (fail) ; Don't inject an empty WHERE
		 `(select (columns ,@(map loop tables))
			  (from ,@(map loop from-specs))
			  (where (and ,@(map (lambda (alias) (rewrite-tables scope `((,table . ,(cdr alias))))) aliases)))
			  ,@(map loop more)))))
	  (other (map loop other)))))

;; Find all tables used in a query.  Returns an list of conses: ((table . alias) ...)
;; A table may occur more than once!
(define (find-tables ssql)
  (let loop ((expect-table? #f)
	     (tables '())
	     (ssql ssql))
    (match ssql
	   ((? symbol?)
	    (if expect-table?
		(cons (cons ssql ssql) tables)
		tables))
	   (('as (? symbol? table) alias)
	    (cons (cons table alias) tables))
	   ((or ('from rest ...)
		('join _ rest ...))
	    (append (apply append (map (lambda (tbl) (loop #t '() tbl)) rest)) tables))
	   ((head tail ...)
	    (append (loop #f tables head) (loop #f tables tail)))
	   (_ tables))))

(define (ssql-compose connection ssql clauses)
  (call-with-sql-engine connection
    (lambda (engine)
      (cons (car ssql)
            (fold-right (lambda (clause ssql)
                          (let ((target-clause (alist-ref (car clause) ssql)))
                            (if target-clause
                                (alist-update! (car clause)
                                               (engine 'merge-clauses target-clause clause)
                                               ssql)
                                (engine 'insert-clause clause ssql))))
                        (cdr ssql)
                        clauses)))))


)