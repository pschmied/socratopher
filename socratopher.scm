;;; Socratopher
(use bindings)

(require-library phricken)
(import (prefix phricken ph:))

(require-library http-client)
(import (prefix http-client http:))

(require-library medea)
(import (prefix medea medea:))

;; Parsing JSON arrays as lists instead of vectors
(define array-as-list-parser
  (cons 'array (lambda (x) x)))

;; like alist-refsym, but more forgiving of json turned into lists
(define (alist-refsym key list)
  (let ((val (alist-ref key list)))
    (cond
     ((string? val) val) 
     ((symbol? val) (symbol->string val))
     ((list? val) (string-join val " ")))))

;; Socrata access functions

(define (list-domains)
  (vector->list
   (cdr
    (assoc 'results
           (http:with-input-from-request
            "https://api.us.socrata.com/api/catalog/v1/domains"
            #f
            medea:read-json)))))

(define (list-datasets domainstring)
  (vector->list
   (cdr
    (assoc
     'results
     (http:with-input-from-request
      (string-append "https://api.us.socrata.com/api/catalog/v1?limit=10000&domains="
                     domainstring)
      #f
      medea:read-json)))))

(define (get-metadata 4x4)
  (filter
   pair?
   (flatten
    (vector->list
     (alist-ref
      'results
      (http:with-input-from-request
       (string-append "https://api.us.socrata.com/api/catalog/v1?ids=" 4x4)
       #f
       medea:read-json))))))


(define (domain->sgm domain)
  (bind-let
   ((((d . dstring) (c . count)) domain))
   `(1 ,(string-append dstring " - (" (number->string count) " items)") ,dstring)))

(define (dataset->sgm dataset domain)
  (let* ((attribs (flatten (cdr (assoc 'resource dataset))))
         (name (alist-ref 'name attribs))
         (id (alist-ref 'id attribs)))
    `(1 ,(string-append id " - " name) ,(string-append domain "/" id))))

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
  (let* ((domain (ph:request-selector req))
         (->sgm (lambda (x) (dataset->sgm x domain))))
    (ph:send-entries
     `((i "Dataset listing")
       (i "----------------------------------")
       (i)))
    (ph:send-entries (map ->sgm (list-datasets domain)))))

(define (handle-dataset-detail req)
  (let* ((selector-components (string-split (ph:request-selector req) "/"))
         (domain (first selector-components))
         (4x4 (last selector-components))
         (metadata (get-metadata 4x4))
         (col-names (vector->list (alist-ref 'columns_field_name metadata)))
         (col-types (vector->list (alist-ref 'columns_datatype metadata)))
         (col-strings (map (lambda (x) (string-join x " :: "))
                           (zip col-names col-types))))
    (ph:send-entries
     `((i ,(string-append "Name:        "
                          (alist-refsym 'name metadata)))
       (i ,(string-append "Description: "
                          (alist-refsym 'description metadata)))
       (i ,(string-append "Created:     "
                          (alist-refsym 'createdAt metadata)))
       (i ,(string-append "Updated:     "
                          (alist-refsym 'updatedAt metadata)))
       (i)
       (i "Columns:")
       (i "----------------------------------")))
    (ph:send-entries
     (map (lambda (x) `(i ,x)) col-strings))))

(define handlers
  `(,(ph:match-selector "" handle-root)
    ,(ph:match-selector "/domains" handle-domain-list)
    ,(ph:match-selector ".+\\.[^/]+" handle-dataset-list)
    ,(ph:match-selector ".+\\..+/[a-z0-9]{4}-[a-z0-9]{4}$"
                        handle-dataset-detail)))


;; Start the server with appropriate parameters
(parameterize
    ((ph:port 7070)
     (ph:handlers handlers))
  (ph:start-server! #f))
