;; Postgresql implementation of ssql
(module ssql-pgsql

(*pgsql-translator*)

(import chicken scheme)
(use ssql postgresql foops)

(define *pgsql-translator* 
  (let ((type->sql-converters 
         `((,boolean? . boolean->sql)
           ,@(*ansi-translator* 'type->sql-converters))))

    (derive-object (*ansi-translator*)
                   ((escape-string string)
                    (escape-string (ssql-connection) string))

                   ((boolean->sql boolean)
                    (if boolean "'t'" "'f'"))

                   ((type->sql-converters) type->sql-converters))))

(register-sql-engine! connection? *pgsql-translator*)

)