(use test)
(import ssql)

(test-begin "selects")
(test "Simple query"
  "SELECT actors.firstname, actors.lastname FROM actors"
  (ssql->sql #f `(select (columns actors.firstname actors.lastname)
                   (from actors))))
(test "Many columns"
  "SELECT actors.id, actors.firstname, actors.lastname, roles.character, roles.movie_id AS movie FROM actors, roles"
  (ssql->sql #f `(select (columns (col actors id firstname lastname) (col roles character (as movie_id movie)))
                   (from actors roles))))
(test "Joined query"
  (string-append
   "SELECT actors.firstname, actors.lastname, roles.character, movies.title "

   "FROM ((actors LEFT JOIN roles ON (roles.actor_id = actors.id)) "
   "LEFT JOIN movies ON (movies.id = roles.movie_id))")
  (ssql->sql #f `(select (columns actors.firstname actors.lastname roles.character movies.title)
                   (from (join left
                               (join left actors roles
                                     (on (= roles.actor_id actors.id)))
                               movies
                               (on (= movies.id roles.movie_id)))))))

(test "Order"
  "SELECT lastname, firstname FROM people ORDER BY lastname DESC, firstname"
  (ssql->sql #f '(select (columns lastname firstname) (from people) (order (desc lastname) firstname))))

(test-end "selects")