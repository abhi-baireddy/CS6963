#lang racket

(require "helpers.rkt")
(require "constants.rkt")
(require json)
(provide (all-defined-out))

;;build-step takes new player positions and picked player, and randomly selects a valid neighbor and build there
;;buiding simply increments the level of the selected postion
(define (build-step updated-players new-position-of-picked-token spaces turn)
  (letrec ([picked-token-neighbors (valid-neighbors new-position-of-picked-token
                                                    (append (player-tokens updated-players FIRST)
                                                            (player-tokens updated-players SECOND))
                                                    spaces)]
           [build-at (list-ref picked-token-neighbors (random 0 (length picked-token-neighbors)))] 
           [new-spaces  (for/list ([i (in-range 0 MAX_ROW)])
                          (for/list ([j (in-range 0 MAX_COL)])
                            (if (and (= i (sub1 (first build-at)))
                                     (= j (sub1 (second build-at))))
                                (add1 (val-at spaces (add1 i) (add1 j)))
                                (val-at spaces (add1 i) (add1 j)))))])
    (write-json (make-board updated-players new-spaces (add1 turn)))
    (flush-output)))