#lang racket
(require "helpers.rkt")
(require "constants.rkt")
(require "build-helpers.rkt")

(provide (all-defined-out))

(define (build-without-cards moved-token cards players spaces turn)
  (define potential-build-posns (valid-build-neighbors moved-token
                                                       players
                                                       spaces))
  
  (define level-three-posns (filter-by-level potential-build-posns spaces LEVEL-THREE))
  (define level-two-posns (filter-by-level potential-build-posns spaces LEVEL-TWO))
  (define other-posns (append (filter-by-level potential-build-posns spaces LEVEL-ONE)
                              (filter-by-level potential-build-posns spaces LEVEL-ZERO)))

  (cond [(not (empty? level-three-posns)) (build/write level-three-posns
                                                       (reverse cards)
                                                       (reverse players)
                                                       spaces
                                                       turn)]
        [(not (empty? level-two-posns)) (begin
                                          (define safe-posns (filter (lambda (posn)
                                                                       (not (reachable-by-opponent? posn
                                                                                                    players
                                                                                                    spaces)))
                                                                     level-two-posns))
                                          (cond [(not (empty? safe-posns)) (build/write safe-posns
                                                                                        (reverse cards)
                                                                                        (reverse players)
                                                                                        spaces
                                                                                        turn)]
                                                [else (build/write other-posns
                                                                   (reverse cards)
                                                                   (reverse players)
                                                                   spaces
                                                                   turn)]))]
        [else (build/write other-posns
                           (reverse cards)
                           (reverse players)
                           spaces
                           turn)]))