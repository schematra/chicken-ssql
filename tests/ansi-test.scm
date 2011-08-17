(use test)
(import ssql)

(test-group "selects"
  (test "simple query"
    "SELECT actors.firstname, actors.lastname FROM actors"
    (ssql->sql #f `(select (columns actors.firstname actors.lastname)
                     (from actors))))
  (test "many columns"
    "SELECT actors.id, actors.firstname, actors.lastname, roles.character, roles.movie_id AS movie FROM actors, roles"
    (ssql->sql #f `(select (columns (col actors id firstname lastname) (col roles character (as movie_id movie)))
                     (from actors roles))))
  (test "joined query"
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

  (test "order"
    "SELECT lastname, firstname FROM people ORDER BY lastname DESC, firstname"
    (ssql->sql #f '(select (columns lastname firstname) (from people) (order (desc lastname) firstname)))))

(test-group "updates"
  (test "simple case"
    "UPDATE actors SET firstname = 'Rube', lastname = 'Goldberg'"
    (ssql->sql #f '(update actors (set (firstname "Rube") (lastname "Goldberg")))))

  (test "with condition"
    "UPDATE actors SET firstname = 'Felix' WHERE (lastname = 'Winkelmann')"
    (ssql->sql #f '(update actors (set (firstname "Felix")) (where (= lastname "Winkelmann"))))))

(test-group "inserts"
  (test "with sub-queries"
    "INSERT INTO roles (character, movie_id, actor_id) VALUES ('Dr. Hasenbein', (SELECT id FROM movies WHERE (title = 'Praxis Dr. Hasenbein')), (SELECT id FROM actors WHERE ((firstname = 'Helge') AND (lastname = 'Schneider'))))"
    (ssql->sql #f '(insert (into roles) (columns character movie_id actor_id)
                           (values "Dr. Hasenbein"
                                   (select (columns id) (from movies) (where (= title "Praxis Dr. Hasenbein")))
                                   (select (columns id) (from actors) (where (and (= firstname "Helge")
                                                                                  (= lastname "Schneider"))))))))

  (test "multiple records using vectors for the records"
    "INSERT INTO actors (firstname, lastname) VALUES ('Sylvester', 'Stallone'), ('Arnold', 'Schwarzenegger')"
    (ssql->sql #f '(insert (into actors) (columns firstname lastname) #("Sylvester" "Stallone") #("Arnold" "Schwarzenegger"))))

  (test "without explicit columns"
    "INSERT INTO actors VALUES ('Marlon', 'Brando')"
    (ssql->sql #f '(insert (into actors) (values "Marlon" "Brando")))))

(test-group "syntax"
  (test "set literals"
    "SELECT one, two FROM (1, 2)"
    (ssql->sql #f '(select (columns one two) (from #(1 2))))))