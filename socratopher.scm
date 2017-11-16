;;; Socratopher
(require-library phricken)
(import (prefix phricken ph:))

;; Handlers

;; Site root
(define (handle-root req)
  (ph:send-entries
   `((i "You are home")
     (i "----------------------------------")
     (i)
     (1 "Foo information" "/foo")))
  (ph:send-lastline))

(define (handle-foo req)
  (ph:send-entries
   `((i "This is a thing that says foo")
     (i "----------------------------------")
     (i)
     (1 "back home" "")))
  (ph:send-lastline))

(define handlers
  `(,(ph:match-selector "" handle-root)
    ,(ph:match-selector "/foo" handle-foo)))


;; Start the server with appropriate parameters
(parameterize
    ((ph:port 7070)
     (ph:handlers handlers))
  (ph:start-server! #f))
