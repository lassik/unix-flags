#! /usr/bin/env gosh

;;; Turn the JSON database into a HTML page.

(import (scheme base) (scheme char) (scheme file))
(import (scheme list) (scheme sort) (srfi 13))
(import (gauche base) (rfc json) (sxml serializer))

(define (write-line s) (write-string s) (newline))

(define (flag-class flag)
  (let ((purposes (vector->list (cdr flag))))
    (cond ((not (= 1 (length purposes)))
           "conflicting")
          ((let ((purpose (car purposes)))
             (not (not (assoc "POSIX" (cdr (assoc "os" purpose))))))
           "posix")
          (else
           "nonposix"))))

(define (flag->td flag)
  `(td (@ (class ,(string-join `("flag" ,(flag-class flag)) " ")))
       ,(car flag)))

(let ((commands (with-input-from-file "flags.json" parse-json)))
  (with-output-to-file "flags.html"
    (lambda ()
      (write-line
       (srl:sxml->html
        `(html
          (head
           (title "Unix flags")
           (style ,(string-append
                    "body { font-family: sans-serif; }"
                    "table, tr, td { border: 1px solid black; }"
                    "td.flag { font-family: monospace; font-weight: bold; }"
                    ".posix { background-color: lightgreen; }"
                    ".nonposix { background-color: gainsboro; }"
                    ".conflicting { background-color: pink; }"
                    )))
          (body
           (h1 "Unix flags")
           (table
            (tr (td (@ (class "posix"))
                    "POSIX standard, no conflicting purposes"))
            (tr (td (@ (class "nonposix"))
                    "Non-standard, no conflicting purposes"))
            (tr (td (@ (class "conflicting"))
                    "Conflicting purposes")))
           ,@(append-map (lambda (command)
                           (let ((flags (cdr command)))
                             `((h2 ,(car command))
                               (table
                                ,@(map (lambda (flag)
                                         `(tr ,(flag->td flag)))
                                       (list-sort (lambda (flag1 flag2)
                                                    (string-ci<? (car flag1)
                                                                 (car flag2)))
                                                  flags))))))
                         commands))))))))
