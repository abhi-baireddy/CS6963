#lang racket
(require json)
(require "better-move.rkt")
(require "constants.rkt")
(require "helpers.rkt")


;;generate a random position 
(define (setup players)
  ;;players is either empty or a list of single list which is again a list of two positions
  (let* ([other-player-posns (cond [(empty? players) players]
                                   [else (first players)])]
         [first-pos (random-pos other-player-posns)]
         [second-pos (random-pos (append other-player-posns (list first-pos)))]
         [new-pos (list first-pos second-pos)])

    (write-json(append players (list new-pos)))
    (flush-output)))

;;game init
(define (run-game)
  (local [(define input-board (read-json))]
    (cond
      [(list? input-board) (setup input-board)] 
      [else (move-step (hash-ref input-board 'players) 
                       (hash-ref input-board 'spaces)
                       (hash-ref input-board 'turn))])))

;;game loop
(for ([i (in-naturals)])
  (run-game)
  (flush-output))  