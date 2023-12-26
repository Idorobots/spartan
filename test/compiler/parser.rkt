#lang racket

;; Parser tests.

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/env.rkt")
(require "../../src/compiler/pass.rkt")
(require "../../src/compiler/errors.rkt")
(require "../../src/compiler/modules.rkt")
(require "../../src/compiler/passes/parser.rkt")
(require "../../src/compiler/utils/io.rkt")

(define (p string)
  (env-get ((pass-transform parse)
            (env 'input string
                 'module "test"
                 'errors '()))
           'ast))

(define (pe string)
  (map (lambda (e)
         (format "~a ~a ~a"
                 (compilation-error-what e)
                 (car (compilation-error-location e))
                 (cdr (compilation-error-location e))))
       (env-get ((pass-transform parse)
                 (env 'input string
                      'module "test"
                      'errors '()))
                'errors)))

(describe
 "parser"
 (it "`expand-structure-refs` expands symbol into proper structure accessors"
     (let ((loc (location 5 23)))
       (assert (expand-structure-refs loc 'foo '(bar))
               (generated
                (make-ast-primop-app loc
                                     '&structure-ref
                                     (list (make-ast-symbol loc 'foo)
                                           (generated
                                            (make-ast-quote loc
                                                            (make-ast-symbol loc 'bar)))))))
       (assert (expand-structure-refs loc 'foo '(bar baz faz))
               (generated
                (make-ast-primop-app loc
                                     '&structure-ref
                                     (list (generated
                                            (make-ast-primop-app loc
                                                                 '&structure-ref
                                                                 (list (generated
                                                                        (make-ast-primop-app loc
                                                                                             '&structure-ref
                                                                                             (list (make-ast-symbol loc 'foo)
                                                                                                   (generated
                                                                                                    (make-ast-quote loc
                                                                                                                    (make-ast-symbol loc 'bar))))))
                                                                       (generated
                                                                        (make-ast-quote loc
                                                                                        (make-ast-symbol loc 'baz))))))
                                           (generated
                                            (make-ast-quote loc
                                                            (make-ast-symbol loc 'faz)))))))))

 (it "parses simple expressions"
     (assert (p "foo")
             (make-ast-symbol (location 0 3) 'foo))
     (assert (p "(define (foo x) 23)")
             (make-ast-list (location 0 19)
                            (list (make-ast-symbol (location 1 7) 'define)
                                  (make-ast-list (location 8 15)
                                                 (list (make-ast-symbol (location 9 12) 'foo)
                                                       (make-ast-symbol (location 13 14) 'x)))
                                  (make-ast-number (location 16 18) 23))))
     (assert (p "(define (oof x) 32)")
             (make-ast-list (location 0 19)
                            (list (make-ast-symbol (location 1 7) 'define)
                                  (make-ast-list (location 8 15)
                                                 (list (make-ast-symbol (location 9 12) 'oof)
                                                       (make-ast-symbol (location 13 14) 'x)))
                                  (make-ast-number (location 16 18) 32)))))

 (it "parses structure refs"
     (assert (p "foo.bar")
             (expand-structure-refs (location 0 7) 'foo '(bar)))
     (assert (p "foo.bar.baz.faz")
             (expand-structure-refs (location 0 15) 'foo '(bar baz faz))))

 (it "parses strings"
     (assert (p "\"this is a string\"")
             (make-ast-string (location 0 18) "this is a string"))
     (assert (p "(define foo \"this is a string\")")
             (make-ast-list (location 0 31)
                            (list (make-ast-symbol (location 1 7) 'define)
                                  (make-ast-symbol (location 8 11) 'foo)
                                  (make-ast-string (location 12 30) "this is a string")))))

 (it "parses comments"
     (assert (p "(define (foo x) ;; Coments should be removed!
                   true)")
             (make-ast-list (location 0 70)
                            (list (make-ast-symbol (location 1 7) 'define)
                                  (make-ast-list (location 8 15)
                                                 (list (make-ast-symbol (location 9 12) 'foo)
                                                       (make-ast-symbol (location 13 14) 'x)))
                                  (make-ast-symbol (location 65 69) 'true)))))

 (it "handles invalid symbols gracefully"
     (assert (pe "foo.")
             (list "Invalid symbol `foo.` specified at: 0 4"))
     (assert (pe "foo.bar.")
             (list "Invalid symbol `foo.bar.` specified at: 0 8"))
     (assert (pe ".foo")
             (list "Invalid symbol `.foo` specified at: 0 4"))
     (assert (pe "foo..bar")
             (list "Invalid symbol `foo..bar` specified at: 0 8"))
     (assert (pe "...")
             (list "Invalid symbol `...` specified at: 0 3"))
     (assert (pe ".")
             (list "Invalid symbol `.` specified at: 0 1")))

 (it "handles special characters correctly"
     (assert (p "|")
             (make-ast-symbol (location 0 1)
                              '\|)))

 (it "handles unterminated lists gracefully"
     (assert (pe "(")
             (list "Unterminated list, expected a closing `)` to follow: 0 1"))
     (assert (pe "(()")
             (list "Unterminated list, expected a closing `)` to follow: 0 3"))
     (assert (pe "(define (foo x) 23")
             (list "Unterminated list, expected a closing `)` to follow: 0 18"))
     (assert (pe "(define (foo x 23)")
             (list "Unterminated list, expected a closing `)` to follow: 0 18")))

 (it "handles unterminated strings gracefully"
     (assert (pe "\"This is an unterminated string")
             (list "Unterminated string literal, expected a closing `\"` to follow: 0 31"))
     (assert (pe "(define foo \"This is an unterminated string)")
             (list "Unterminated list, expected a closing `)` to follow: 0 44"
                   "Unterminated string literal, expected a closing `\"` to follow: 12 44")))

 (it "handles unterminated comments"
     (assert (pe "(define (foo x) ;; Coments should be removed!")
             (list "Unterminated list, expected a closing `)` to follow: 0 45")))

 (it "handles unterminated quotation"
     (assert (pe "'")
             (list "No expression following `'`: 0 1"))
     (assert (pe "`")
             (list "No expression following ```: 0 1"))
     (assert (pe ",")
             (list "No expression following `,`: 0 1"))
     (assert (pe ",@")
             (list "No expression following `,@`: 0 2"))
     (assert (pe "(define (foo x) ')")
             (list "No expression following `'`: 16 17")))

 (it "handles extra unmatched tokens"
     (assert (pe ")")
             (list "Unmatched `)`, expected an opening `(` to come before: 0 1"))
     (assert (pe "(define (foo x) x))")
             (list "Unmatched `)`, expected an opening `(` to come before: 18 19"))
     (assert (pe "(define (foo x)) x)")
             (list "Unmatched `)`, expected an opening `(` to come before: 18 19")))

 (it "parses all the examples"
     (define (expected-read input)
       (with-input-from-string input
         (lambda ()
           (read))))
     (map (lambda (filename)
            (let ((contents (slurp filename)))
              (assert (ast->plain (p contents))
                      (expected-read contents))))
          (filter (lambda (filename)
                    (not (member filename (list "test/sprtn/logger.sprtn"
                                                "test/sprtn/amb2.sprtn"))))
                  (filter (lambda (filename)
                            (string-suffix? filename ".sprtn"))
                          (map (lambda (path)
                                 (string-append "test/sprtn/"
                                                (path->string path)))
                               (directory-list "test/sprtn/")))))))
