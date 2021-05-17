#lang racket
(require "../helpers.rkt")
(require "../constants.rkt")
(require "../move-without-cards.rkt")

(provide (all-defined-out))
;; A token can win either by moving up to level 3 or by moving down two or more levels.
;; (Moving down three levels is possible if a token was pushed by a Minotaur.)
(define (move-pan cards players spaces turn)
  ;; if any of the two tokens have a neighbor that is at level three
  ;; or a neighbor that is two levels below current level, then Pan can be used
  ;; get all unoccupied neighbors of both tokens; don't care about level difference
  (define-values (first-token second-token) (get-player-tokens FIRST players))
  
  ;; get neihbors that are not level 4 and not occupied by other tokens
  ;; and then keep only those that are either of level 3 or of two or more levels
  ;; below the current level
  (define first-token-unoccupied-neighbors (filter (lambda (posn)
                                                     (or (= LEVEL-THREE (level-of posn spaces))
                                                         (>= 2 (- (level-of first-token)
                                                                  (level-of posn)))))
                                                   (get-unoccupied-neighbors first-token
                                                                             players
                                                                             spaces)))
  (define second-token-unoccupied-neighbors (filter (lambda (posn)
                                                      (or (= LEVEL-THREE (level-of posn spaces)) 
                                                          (>= 2 (- (level-of second-token)
                                                                   (level-of posn)))))
                                                    (get-unoccupied-neighbors second-token
                                                                              players
                                                                              spaces)))
  (define possible-win-posns (set->list (set-union (list->set first-token-unoccupied-neighbors)
                                                   (list->set second-token-unoccupied-neighbors))))
  ;; if there are no win positions then proceed as in the no cards version
  (cond [(empty? possible-win-posns) (move-without-cards cards
                                                         players
                                                         spaces
                                                         turn)]
        ;; if there is at least one win position, move there and win
        [else (move-and-win (select-random possible-win-posns)
                            players
                            cards
                            turn)]))
