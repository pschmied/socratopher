;;; Socratopher
(require-extension phricken)


;; Handlers

;; Site root
(define (handle-root req)
  (send-entries
   `((i "You are home")
     (i "----------------------------------")
     (i)
     (l "This is a link" "/foo")))
  (send-lastline))

(define (handle-foo req)
  (send-entries
   `((i "This is a thing that says foo")
     (i "----------------------------------")
     (i)
     (l "back home" "")))
  (send-lastline))

(define handlers
  `(,(match-resource "" handle-root)
    ,(match-resource "/foo" handle-foo)))

;; Server configuration
(set! port (make-parameter 7070))
(set! handlers (make-parameter handlers))

(start-server! #f)
