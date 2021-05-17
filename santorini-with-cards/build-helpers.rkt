#lang racket

(require json)
(require "helpers.rkt")
(require "constants.rkt")
(provide (all-defined-out))

;; wrapper for writing board state after building
(define (build/write possible-build-posns cards players spaces turn)
  (write-json (make-board cards
                          players
                          (build-at (select-random possible-build-posns)
                                    spaces)
                          (add1 turn)))
  (flush-output))

;; wrapper for writing board state
(define (write-board cards players spaces turn)
  (write-json (make-board cards
                          players
                          spaces
                          turn))
  (flush-output))


;; increments the level of the specified position and returns new spaces state
(define (build-at pos spaces)
  (for/list ([i (in-range 0 MAX_ROW)])
    (for/list ([j (in-range 0 MAX_COL)])
      (if (and (= i (sub1 (first pos)))
               (= j (sub1 (second pos))))
          (add1 (val-at spaces (add1 i) (add1 j)))
          (val-at spaces (add1 i) (add1 j))))))  

;; completes the tower at pos -- called when Atlas card is used
(define (complete-tower-at pos spaces)
  (for/list ([i (in-range 0 MAX_ROW)])
    (for/list ([j (in-range 0 MAX_COL)])
      (if (and (= i (sub1 (first pos)))
               (= j (sub1 (second pos))))
          COMPLETE
          (val-at spaces (add1 i) (add1 j))))))

;; give a list of build posns and spaces, this builds at two of the build posns
(define (demeter-build-at build-posns spaces)
  (define first-build-posn (select-random build-posns))
  ;; demeter cannot build at same posn so remove it from list of build-posns
  (define second-build-posn (select-random (remove first-build-posn build-posns)))
  (build-at second-build-posn
            (build-at first-build-posn spaces)))

;; builds twice at specified position; called when player has Hephastus card
(define (hephastus-build-at build-posn spaces)
  (build-at build-posn
            (build-at build-posn
                      spaces)))