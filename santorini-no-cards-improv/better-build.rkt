#lang racket

(require "helpers.rkt")
(require "constants.rkt")
(require json)
(provide (all-defined-out))

;;build-step takes new player positions and picked player, and randomly selects a valid neighbor and build there
;;buiding simply increments the level of the selected postion
(define (build-step players moved-token spaces turn)
  
  (define current-player-tokens (player-tokens players FIRST))
  (define opponent-tokens (player-tokens players SECOND))
  (define potential-build-posns (filter (lambda (neighbor)
                                          (and (not-occupied? neighbor players)
                                               (not-complete? neighbor spaces)))
                                        (filter inside-board?
                                                (get-neighbors moved-token))))
  
  (define level-three-posns (filter-by-level potential-build-posns spaces 3))
  (define level-two-posns (filter-by-level potential-build-posns spaces 2))
  (define other-posns (append (filter-by-level potential-build-posns spaces 0)
                              (filter-by-level potential-build-posns spaces 1)))

  (cond [(not (empty? level-three-posns)) (write-json (make-board players
                                                      (build-at (select-random level-three-posns)
                                                                spaces)
                                                      (add1 turn)))]
        [(not (empty? level-two-posns)) (begin
                                          (define safe-posns (filter (lambda (posn)
                                                                       (not (reachable-by-opponent? posn players spaces)))
                                                                     level-two-posns))
                                          (cond [(not (empty? safe-posns)) (write-json (make-board players
                                                                                       (build-at (select-random safe-posns)
                                                                                                 spaces)
                                                                                       (add1 turn)))]
                                                [else (write-json (make-board players
                                                                  (build-at (select-random other-posns)
                                                                            spaces)
                                                                  (add1 turn)))]))]
        [else (write-json (make-board players
                          (build-at (select-random other-posns)
                                    spaces)
                          (add1 turn)))])
  (flush-output))




;; increments the level of the specified position and returns new spaces state
(define (build-at pos spaces)
  (for/list ([i (in-range 0 MAX_ROW)])
    (for/list ([j (in-range 0 MAX_COL)])
      (if (and (= i (sub1 (first pos)))
               (= j (sub1 (second pos))))
          (add1 (val-at spaces (add1 i) (add1 j)))
          (val-at spaces (add1 i) (add1 j))))))  
