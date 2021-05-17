#lang racket
(require json)
(require "helpers.rkt")
(require "constants.rkt")
(require "build-with-cards.rkt")
(require "build-without-cards.rkt")
(require "move-without-cards.rkt")
(require "build-helpers.rkt")
(require "cards/apollo.rkt")
(require "cards/artemis.rkt")
(require "cards/minotaur.rkt")
(require "cards/prometheus.rkt")
(require "cards/pan.rkt")

(provide (all-defined-out))


;; extract board state and move
(define (make-move input-board)
  (let* ([players (hash-ref input-board 'players)]
         [player-tokens (get-both-player-tokens players)]
         [player-cards (get-both-player-cards players)]
         [spaces (hash-ref input-board 'spaces)]
         [turn (hash-ref input-board 'turn)])
    (move-step player-cards player-tokens spaces turn)))


;; move with cards -- proceed as usual to get a list of possible moves
;; if the current player has a move card, then extend the list of possible moves
(define (move-step cards players spaces turn)
  (define current-player-card (first cards))
  ;; get list of all possible move positions
  (define possible-move-posns (get-all-neighbors-of-player FIRST players spaces))
  
  ;; always move to a level three pos and win if it is possible
  (define levels-of-possible-move-posns (map (lambda (posn)
                                               (level-of posn spaces))
                                             possible-move-posns))
  (define index-of-win-posn (index-of levels-of-possible-move-posns LEVEL-THREE))
  ;; if there is a neighbor position whose level is three, move there and win
  ;; otherwise check if your card is a move card
  ;; if it is a move card, decide whether or not to use the card by calling use-card?
  ;; if use-card? says no, call move-without-card; otherwise call move-with-card
  (cond [(number? index-of-win-posn) (move-and-win (list-ref possible-move-posns index-of-win-posn)
                                                   cards
                                                   players
                                                   spaces
                                                   turn)]
        
        [else (if (list? (member current-player-card move-cards))
                  (move-with-cards cards players spaces turn)
                  (move-without-cards cards players spaces turn))]))


;;move-step-with-card is called when the player decides to use his card (if he has a move card)
;; each of the move-card methods will return true of false indicating wheter a move has been made
;; by using the card
;; if the card has been used, this method need not do anything
;; if it's been decided that the card not be used, this method must call move-without-card
(define (move-with-cards cards players spaces turn)
  (define current-player-card (first cards))
  (case current-player-card
    [("Pan") (move-pan cards players spaces turn)] ;;Done
    [("Apollo") (move-apollo cards players spaces turn)]
    [("Artemis") (move-artemis cards players spaces turn)]
    [("Minotaur") (move-minotaur cards players spaces turn)]
    [("Prometheus") (move-prometheus cards players spaces turn)]))






  












