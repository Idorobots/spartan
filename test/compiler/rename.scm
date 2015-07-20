;; Renaming things for safety.

(assert (symbol->safe 'foo) '__foo)
(assert (symbol->safe 'foo23) '__foo23)
(assert (symbol->safe 'foo!) '__fooBANG)
(assert (symbol->safe 'symbol->safe) '__symbol_GREATERsafe)
