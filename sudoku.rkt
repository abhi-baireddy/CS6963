#lang racket
(define M 3)
(define N 2)

;;initialize all cells of M * N sudoku board to some random number
(define (initialize-board M N n board column)
  (cond
    ((= 0 M) board)
    ((= 0 n) (initialize-board (- M 1) N N (append board (list column)) '()))
    ((> n 0) (initialize-board M N (- n 1) board (cons 0 column))))
  )
;;check if a number is in a list
(define (contains? collection val)
  (cond
    ((empty? collection) #f)
    ((= (first collection) val) #t)
    (else (contains? (rest collection) val))
    )
  )

;;get grid number of cell [r, c] of board of size M X N
(define (get-grid-number r c M N)
  (+ (* (quotient r N) N) (quotient c M))
  )

;;get value at cell [r,c] in board
(define (val-at board r c)
  (list-ref (list-ref board r) c)
  )

;;print an M X N board
(define (print-board board dim M N r c)
  (cond
    ((= r dim) (display "\n"))
    ((and (= c dim) (< (quotient r N) (quotient (+ r 1) N))) (display "\n\n") (print-board board dim M N (+ 1 r) 0))
    ((= c dim) (display "\n") (print-board board dim M N (+ 1 r) 0))
    ((< (quotient c M) (quotient (+ c 1) M)) (display (val-at board r c)) (display "  ") (print-board board dim M N r (+ 1 c)))
    (else (display (val-at board r c)) (display " ") (print-board board dim M N r (+ 1 c)))
    )
  )


;;set board[row][col] to val
(define (place-val board row col val)
  (list-set board row (list-set (list-ref board row) col val))
  )

;;returns true if val is not in row #row of board; false otherwise
(define (row-safe? val row board)
  (not (contains? (list-ref board row) val))
  )

;;returns false if val is not in column #col of board; false otherwise
(define (col-safe? val col row dim board)
  (cond
    ((= row dim) #t)
    (else
     (and (not (= (val-at board row col) val)) (col-safe? val col (+ row 1) dim board))
    )
  )
 )

;;returns true if val at board[row][col] is not already in the same grid as the cell [row, col]
(define (grid-safe? val grid-num r c M N board)
  (cond
    ((= r (* M N)) #t)
    ((= c (* M N)) (grid-safe? val grid-num (+ 1 r) 0 M N board))
    ((and (= grid-num (get-grid-number r c M N)) (= val (val-at board r c))) #f)
    (else (and #t (grid-safe? val grid-num r (+ 1 c) M N board)))
  )
 )

;;returns true if the given val is row, col, and grid safe
(define (is-safe? val board r c M N)
  (and (row-safe? val r board)
       (and (col-safe? val c 0 (* M N) board)
            (grid-safe? val (get-grid-number r c M N) 0 0 M N board)))
 )
;;tries to fill a cell with all possible values
(define (try-vals val board r c M N)
   (cond
     ((= val (+ 1 (* M N))) #f)
     ((is-safe? val board r c M N)
      (if (solve-sudoku (place-val board r c val) r (+ 1 c) M N)
          #t
          (try-vals (+ val 1) (place-val board r c 0) r c M N)
        )
      )
     (else (try-vals (+ val 1) board r c M N))
    )
  )

;;erases count number of cells from board
(define (erase-cells count board dim)
  (cond
  ((= 0 count) board)
  (else
   (local [(define r (random 0 dim)) (define c (random 0 dim))]
     (if (= 0 (val-at board r c))
         (erase-cells count board dim)
         (erase-cells (- count 1) (place-val board r c 0) dim)))
   ))
  )
;;solves empty or partially filled sudoku board
(define (solve-sudoku board r c M N)
  (cond
    ((= r (* M N)) (begin (display "Solved the board!\n") (print-board board (* M N) M N 0 0) #t))
    ((= c (* M N)) (solve-sudoku board (+ r 1) 0 M N))
    ((not (= 0 (val-at board r c))) (solve-sudoku board r (+ c 1) M N))
    (else (try-vals 1 board r c M N))
   )
 )


;;returns true if all the cells in the first-sub-grid are assigned values
(define (is-grid-full? board r c M N)
  (cond
    ((= r N) #t)
    ((= c M) (is-grid-full? board (+ r 1) 0 M N))
    ((= 0 (val-at board r c)) #f)
    (else (is-grid-full? board r (+ c 1) M N))
   )
  )
;;initializes the first sub-grid with random values
(define (initialize-grid board M N)
  (cond
    ((is-grid-full? board 0 0 M N) board)
    (else
      (local [(define r (random 0 N)) (define c (random 0 M)) (define val (random 1 (+ 1 (* M N))))]
        (if (grid-safe? val 0 0 0 M N board)
            (if (= 0 (val-at board r c)) (initialize-grid (place-val board r c val) M N) (initialize-grid board M N))
            (initialize-grid board M N))
        )
      
      )
    )
  )





(define dim (* M N))
(define board (initialize-board dim dim dim '() '()))
;;(define board (place-val (initialize-board dim dim dim '() '()) (random 0 dim) (random 0 dim) (random 1 (+ dim 1))))
;;(print-board board dim M N 0 0)
(define test-board
  (list
   (list 5 3 0 0 7 0 0 0 0)
   (list 6 0 0 1 9 5 0 0 0)
   (list 0 9 8 0 0 0 0 6 0)
   (list 8 0 0 0 6 0 0 0 3)
   (list 4 0 0 8 0 3 0 0 1)
   (list 7 0 0 0 2 0 0 0 6)
   (list 0 6 0 0 0 0 2 8 0)
   (list 0 0 0 4 1 9 0 0 5)
   (list 0 0 0 0 8 0 0 7 9)
   ))

(solve-sudoku test-board 0 0 M N)
;;(solve-sudoku board 0 0 M N)

(define test-board2 (initialize-grid board M N))

(print-board test-board2 (* M N) M N 0 0)

;;(define solved-board (solve-sudoku test-board2 0 0 M N))
;;(print-board solved-board (* M N) M N 0 0)
;;(define new-board (erase-cells (floor (* (* M N) 0.6)) solved-board (* M N)))