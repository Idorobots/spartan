;; Continuation Passing Style Conversion

;; Simple CPC works.
(assert (cpc-simple 'foo) (symbol->safe 'foo))
(assert (cpc 'foo id) (symbol->safe 'foo))

(assert (cpc-simple 23) 23)
(assert (cpc 23 id) 23)

(assert (cpc-simple '()) '())
(assert (cpc '() id) '())

(assert (cpc-simple "hurr") "hurr")
(assert (cpc "hurr" id) "hurr")

(assert (cpc-simple '(quote 23)) ''23)
(assert (cpc ''23 id) ''23)
