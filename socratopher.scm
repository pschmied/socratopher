;;; Socratopher
(use bindings)

(require-library phricken)
(import (prefix phricken ph:))

(require-library http-client)
(import (prefix http-client http:))

(require-library json)
(import (prefix json json:))


;; Socrata access functions

(define (list-domains)
  (cdr
   (car 
    (vector->list
     (http:with-input-from-request
      "https://api.us.socrata.com/api/catalog/v1/domains"
      #f
      json:json-read)))))


(define (domain->sgm domain)
  (bind-let
   ((((d . dstring) (c . count)) domain))
   `(1 ,(string-append dstring " - (" (number->string count) " items)") ,dstring)))

(define (list-datasets domainstring)
  (cdr
   (vector-ref
    (http:with-input-from-request
     (string-append "https://api.us.socrata.com/api/catalog/v1?domains=" domainstring)
     #f
     json:json-read)
    0)))


;; Handlers

;; Site root handler
(define (handle-root req)
  (ph:send-entries
   `((i "             -------  _         ")
     (i "          --/        / \\_      ")
     (i "         /    ----- /    \\     ")
     (i "       -/   -/     |      \\    ")
     (i "      /    /       /      /     ")
     (i "      |   /    -- /    /-       ")
     (i "     /   /    /  /  /-     \\   ")
     (i "     |   |   /    -    |   |    ")
     (i "     |   |   |     |   |   |    ")
     (i "     |   |   \\     /   |   |   ")
     (i "     \\   \\    \\   /    /   / ")
     (i "      |   \\    ---    /   |    ")
     (i "      \\    \\         /    /   ")
     (i "       -\\   -\\     /-   /-    ")
     (i "         \\    -----    /       ")
     (i "          --\\       /--        ")
     (i " ____       -------         _   ")
     (i "/ ___|  ___   ___ _ __ __ _| |_ __ _       ")
     (i "\\___ \\ / _ \\ / __| '__/ _` | __/ _` |   ")
     (i " ___) | (_) | (__| | | (_| | || (_| |      ")
     (i "|____/ \\___/ \\___|_|  \\__,_|\\__\\__,_| ")
     (i "                                           ")
     (i "   (c)1992 Socrata Data Systems, Inc.")
     (i)
     (i "You are home - Open Data Network")
     (i "----------------------------------")
     (i)
     (1 "Domain list" "/domains")))
  (ph:send-lastline))

;; Domain list
(define (handle-domain-list req)
  (ph:send-entries
   `((i "All domains")
     (i "----------------------------------")
     (i)))
  (ph:send-entries (map domain->sgm (list-domains)))
  (ph:send-lastline))

(define (handle-dataset-list req)
  (ph:send-entries
   `((i "Dataset listing")
     (i "----------------------------------")
     (i req))))


(define handlers
  `(,(ph:match-selector "" handle-root)
    ,(ph:match-selector "/domains" handle-domain-list)))


;; Start the server with appropriate parameters
(parameterize
    ((ph:port 7070)
     (ph:handlers handlers))
  (ph:start-server! #f))
