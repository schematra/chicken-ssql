(import (chicken load) test)

(load-relative "foops-test")
(load-relative "transformations-test")
(load-relative "ansi-test")
(load-relative "composition-test")

(test-exit)