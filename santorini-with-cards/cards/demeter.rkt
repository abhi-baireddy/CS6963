#lang racket

(require "../helpers.rkt")
(require "../constants.rkt")
(require "../build-helpers.rkt")
(require "../build-without-cards.rkt")
(provide (all-defined-out))

;; The moved token can optionally build a second time,
;; but not on the same space as the first build within a turn
(define (build-demeter moved-token cards players spaces turn)
  ;; get all possible build positions of moved-token
  (define possible-build-posns (valid-build-neighbors moved-token
                                                      players
                                                      spaces))
  ;; get level-two posns unreachable by opponent
  (define level-two-safe-neighbors (filter (lambda (neighbor)
                                             (and (reachable-by-opponent? neighbor
                                                                          players
                                                                          spaces)
                                                  (= (level-of neighbor LEVEL-TWO))))
                                           possible-build-posns))
  ;; get all valid neighbors of opponent's tokens
  (define opponent-move-posns (get-all-neighbors-of-player SECOND
                                                           players
                                                           spaces))
  ;; find common posns
  (define common-neighbors-with-opponent (set->list (set-intersect (list->set possible-build-posns)
                                                                   (list->set opponent-move-posns))))
  (define non-level-two-common-neighbors (filter (lambda (neighbor)
                                                   (not (= (level-of neighbor spaces)
                                                           LEVEL-TWO)))
                                                 common-neighbors-with-opponent))
  ;; to minimize opponent's list of possible moves in the next turn
  ;; build on common neighbors that are not at level 2
  ;; filtering out level 2 posns from commmon-neighbors-with-opponent will leave us with
  ;; level 0 or 1 or 3 neighbors that can be reached by opponent
  ;; building on these will make them inaccessible to the the opponent token in the next move
  ;; use this card to maximize win chances in next move or minimize move posns for opponent
  (cond [(> (length level-two-safe-neighbors) 1) (write-board cards
                                                              players
                                                              (demeter-build-at level-two-safe-neighbors)
                                                              (add1 turn))]
        [(> (length non-level-two-common-neighbors) 1) (write-board cards
                                                                    players
                                                                    (demeter-build-at non-level-two-common-neighbors)
                                                                    (add1 turn))]
        [else (build-without-cards moved-token cards spaces players turn)]))

