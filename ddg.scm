(use-modules (web request)
             (web response)
             (web client)
             (web uri)
             (json)) ; maybe allow xml, since it comes with guile?
                     ; whooo no dependencies

(define (zero-click query-string)
  (define uri
    (build-uri 'http
               #:host "api.duckduckgo.com"
               #:path "/"               ; necessary for backwards
                                        ; compatibility with buggy guile
               #:query (form-encode `((q . ,query-string)
                                      (no_redirect . "1")
                                      ;(t . "guile-ddg")
                                      (format . "json")
                                      ;(no_html . "1")
                                      ))
               #:validate? #t)) ; maybe turn off later
  (call-with-values
      (lambda ()
        (http-get uri #:streaming? #t))
    (lambda (response port)
      (unless (= 200 (response-code response))
        (throw 'something-fucked-up))
      ;; Eventually, return a results object of some sort so that we
      ;; can e.g. distinguish useful results from all blanks.
      (json->scm port))))

(define (form-encode form-alist)
  ;; Look Ma, No consistency checks!
  (define (escape str)
    ;; don't get me started on how wrong this is, but it will do for a start
    (uri-encode (string-map (lambda (c) (if (eqv? c #\space) #\+ c)) str)))
  (string-join
   (map (lambda (pair)
          (string-append (escape (symbol->string (car pair))) "=" (escape (cdr pair))))
        form-alist)
   "&"))
