#! /usr/bin/env gosh

;;; Turn the JSON database into a HTML page.

(import (scheme base) (scheme char) (scheme file))
(import (scheme list) (scheme sort) (srfi 13))
(import (only (file util) directory-list)
        (only (rfc json) parse-json)
        (only (sxml serializer) srl:sxml->html))

(define (write-line s) (write-string s) (newline))

(define cmd-dir "cmd/")

(define (list-sort-car less? lis)
  (list-sort (lambda (a b) (less? (car a) (car b)))
             lis))

(define (uppercase-before-lowercase<? a b)
  (if (string-ci=? a b)
      (string<     a b)
      (string-ci<? a b)))

(define flag<? uppercase-before-lowercase<?)
(define command<? string-ci<?)

(define (os<? os1 os2)
  (cond ((string=? "POSIX" os1) #t)
        ((string=? "POSIX" os2) #f)
        (else (string-ci<? os1 os2))))

(define (basename->command basename)
  (and (not (string-prefix? "." basename))
       (string-suffix? ".json" basename)
       (substring basename 0 (- (string-length basename)
                                (string-length ".json")))))

(define (command-names)
  (list-sort command<?
             (map basename->command
                  (filter basename->command
                          (directory-list cmd-dir)))))

(define (command-json-file command) (string-append cmd-dir command ".json"))

(define (flag-class purposes)
  (cond ((not (= 1 (length purposes)))
         "conflicting")
        ((let ((purpose (car purposes)))
           (not (not (assoc "POSIX" (cdr (assoc "os" purpose))))))
         "posix")
        (else
         "nonposix")))

(define (flag->tr flag-spec)
  (let ((flag (car flag-spec))
        (purposes (vector->list (cdr flag-spec))))
    `(tr (td (@ (class ,(string-join `("flag" ,(flag-class purposes)) " ")))
             ,flag)
         (td ,(string-join
               (append-map (lambda (purpose)
                             (let ((os-list
                                    (map car (cdr (assoc "os" purpose)))))
                               (list-sort os<? os-list)))
                           purposes)
               ", ")))))

(let ((commands
       (map (lambda (command)
              (cons command
                    (with-input-from-file (command-json-file command)
                      parse-json)))
            (command-names))))
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
                           (let ((flags (cdr (assoc "flags" (cdr command)))))
                             `((h2 ,(car command))
                               (table
                                ,@(map flag->tr
                                       (list-sort-car flag<? flags))))))
                         (list-sort-car command<? commands)))))))))
