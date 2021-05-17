#lang racket

(require "../helpers.rkt")
(require "../constants.rkt")
(require "../build-helpers.rkt")
(require "../build-without-cards.rkt")
(provide (all-defined-out))

;; The moved token can optionally build a second time,
;;but only on the same space as the first build within a turn,
;; and only if the second build does not reach level 4.
(define (build-hephastus moved-token cards players spaces turn)
  ;; second build should not reach level 4 so cannot build at level 2 spaces
  (define build-posns (valid-build-neighbors moved-token
                                             players
                                             spaces))
  (define level-zero-posns (filter-by-level build-posns spaces LEVEL-ZERO))
  ;; level one positions not reachable by opponent in the next move
  (define safe-level-one-posns (filter (lambda (neighbor)
                                         (not (reachable-by-opponent? neighbor
                                                                      players
                                                                      spaces)))
                                       (filter-by-level build-posns spaces LEVEL-ONE)))
  
  (cond [(not (empty? safe-level-one-posns)) (write-board cards
                                                          players
                                                          (hephastus-build-at (select-random safe-level-one-posns)
                                                                              spaces)
                                                          (add1 turn))]
        [(not (empty? level-zero-posns)) (write-board cards
                                                      players
                                                      (hephastus-build-at (select-random level-zero-posns)
                                                                          spaces)
                                                      (add1 turn))]
        [else (build-without-cards moved-token cards players spaces turn)]))