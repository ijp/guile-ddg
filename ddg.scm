;;; ddg.scm --- Duck Duck Go Api Access

;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Code:
(define-module (ddg)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (ice-9 regex)
  #:use-module (json)                   ; TODO: xml?
  #:export (zero-click))

(define app-name "guile-ddg")

(define* (zero-click query-string #:key (app app-name))
  "Makes a query to the DuckDuckGo instant answer API. Takes a
mandatory QUERY-STRING argument, and returns a json object. You may
pass in an optional #:app keyword argument to specify the application
name the request uses."
  (define uri
    (build-uri 'http
               #:host "api.duckduckgo.com"
               #:path "/"               ; necessary for backwards
                                        ; compatibility with buggy guile
               #:query (form-encode `((q . ,query-string)
                                      (no_redirect . "1")
                                      (t . ,app)
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


(define unreserved-chars
  ;; Differs from usual set by inclusion of #\space
  (string->char-set
   (string-append
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "0123456789"
    "-_.~ ")))

(define (normalize-newlines string)
  "Takes a string and returns a new string where '\r' or '\n' on it's own is replaced with '\r\n'"
  (regexp-substitute/global #f "\r\n|\r|\n" string 'pre "\r\n" 'post))

(define (form-encode form-alist)
  ;; Look Ma, No consistency checks!
  (define (encode str)
    (string-map (lambda (c) (if (eqv? c #\space) #\+ c))
                (uri-encode (normalize-newlines str)
                            #:unescaped-chars unreserved-chars)))

  (define (->string o)
    (object->string o display))

  (string-join
   (map (lambda (pair)
          (string-append (encode (->string (car pair))) "="
                         (encode (->string (cdr pair)))))
        form-alist)
   "&"))
