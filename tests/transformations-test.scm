(use test)
(import ssql)

(test-begin "inspection")
(test "find-tables"
  `((actors . actors) (roles . roles) (movies . m2) (movies . movies))
  (find-tables
   `(select (columns actors.firstname actors.lastname roles.character movies.title)
      (from (join left
                  (join left actors
                        (join inner roles (as movies m2)
                              (on (and (= m2.id roles.movie_id)
                                       (> m2.year 2000))))
                        (on (= roles.actor_id actors.id)))
                  movies
                  (on (= movies.id roles.movie_id)))))))
(test-end "inspection")

(test-begin "scoping")
(test "scope-table"
  `(select (columns actors.firstname actors.lastname roles.character movies.title)
     (from (join left
                 (join left actors
                       (join inner roles (as movies m2)
                             (on (and (= m2.id roles.movie_id)
                                      (> m2.year 2000))))
                       (on (= roles.actor_id actors.id)))
                 movies
                 (on (= movies.id roles.movie_id))))
     (where (and (< (col m2 year) 2005)
                 (< (col movies year) 2005))))
  (scope-table 'movies `(< (col movies year) 2005)
               `(select (columns actors.firstname actors.lastname roles.character movies.title)
                  (from (join left
                              (join left actors
                                    (join inner roles (as movies m2)
                                          (on (and (= m2.id roles.movie_id)
                                                   (> m2.year 2000))))
                                    (on (= roles.actor_id actors.id)))
                              movies
                              (on (= movies.id roles.movie_id)))))))
(test-end "scoping")