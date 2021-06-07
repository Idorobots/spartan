;; Error handling tests

(require "../../src/compiler/passes/errors.rkt")

(describe
 "errors"
 (it "offset->line-and-col translates locations correctly"
     (assert (offset->line-and-col "(define x\n  foo)" 0)
             (list 0 0))
     (assert (offset->line-and-col "(define x\n  foo)" 8)
             (list 0 8))
     (assert (offset->line-and-col "(define x\n  foo)" 10)
             (list 1 0))
     (assert (offset->line-and-col "(define x\n  foo)" 12)
             (list 1 2)))
 (it "normalize-for-display appends a newline if needed"
     (assert (normalize-for-display "some line")
             "some line\n")
     (assert (normalize-for-display "some line\n")
             "some line\n")
     (assert (normalize-for-display "")
             "\n"))
 (it "get-line returns the correct line"
     (assert (get-line "(define x\n  foo)" 0)
             "(define x\n")
     (assert (get-line "(define x\n  foo)" 1)
             "  foo)")
     (assert (get-line "(define x\n  foo)" 5)
             "")))
