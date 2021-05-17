#lang racket
(require "../helpers.rkt")
(require "../constants.rkt")
(require "../build-helpers.rkt")
(require "../build-without-cards.rkt")
(provide (all-defined-out))

;; The build phase can build a space currently at level 0, 1, 2 to make it level 4,
;; instead of building to exactly one more than the space’s current level
(define (build-atlas moved-token cards players spaces turn)
  ;; use this card to reduce the opponent's chance of reaching a level 3 space
  ;; or even worse, leaving it with nowhere to move by completing a space
  ;; by checking if there is at least one common neighbor between the moved-token and the opponent's token

  ;; get build-neighbors of my moved-token
  (define possible-build-posns (valid-build-neighbors moved-token
                                                      players
                                                      spaces))
  ;; get opponent's first and second tokens
  (define-values (opponent-first-token opponent-second-token) (get-player-tokens SECOND players))
  ;; get opponent's first token's possible move positions
  (define opponent-first-token-neighbors (valid-neighbors opponent-first-token
                                                          players
                                                          spaces))
  (define opponent-second-token-neighbors (valid-neighbors opponent-second-token
                                                           players
                                                           spaces))
  ;; find common neighbors between my moved token and opponent's first token
  (define common-neighbors-with-first-token (set->list (set-intersect (list->set opponent-first-token-neighbors)
                                                                      (list->set possible-build-posns))))

  ;; find common neighbors between my moved token and opponent's second token
  (define common-neighbors-with-second-token (set->list (set-intersect (list->set opponent-second-token-neighbors)
                                                                       (list->set possible-build-posns))))
  ;; use Atlas if there is at least one common neighbor with any of the tokens
  ;; don't use if there are no common neighbors
  (cond [(not (empty? common-neighbors-with-first-token)) (write-board (reverse cards)
                                                                       players
                                                                       (complete-tower-at (select-random common-neighbors-with-first-token)
                                                                                          spaces)
                                                                       (add1 turn))]
        [(not (empty? common-neighbors-with-second-token)) (write-board (reverse cards)
                                                                        players
                                                                        (complete-tower-at (select-random common-neighbors-with-second-token)
                                                                                           spaces)
                                                                        (add1 turn))]
        [else (build-without-cards moved-token cards players spaces turn)]))
