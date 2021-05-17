#lang racket

(provide (all-defined-out))


(define MAX_ROW 5)
(define MAX_COL 5)
(define FIRST 0 )
(define SECOND 1)
(define LEVEL-THREE 3)
(define LEVEL-TWO 2)
(define LEVEL-ONE 1)
(define LEVEL-ZERO 0)
(define COMPLETE 4)
(define neighbor-vectors (list (list -1 0)
                               (list -1 1)
                               (list 0 1)
                               (list 1 1)
                               (list 1 0)
                               (list 1 -1)
                               (list 0 -1)
                               (list -1 -1)))
;;move cards
(define APOLLO "Apollo")
(define ARTEMIS "Artemis")
(define MINOTAUR "Minotaur")
(define PROMETHEUS "Prometheus")
(define PAN "Pan")
(define move-cards (list APOLLO ARTEMIS MINOTAUR PAN PROMETHEUS))

;;build cards
(define ATLAS "Atlas")
(define DEMETER "Demeter")
(define HEPHASTUS "Hephastus")
(define build-cards (list ATLAS DEMETER HEPHASTUS))

(define USE-CARD #t)
(define DO-NOT-USE-CARD #f)