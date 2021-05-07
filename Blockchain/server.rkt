#lang racket

(require racket/serialize)
(require "network-constants.rkt")
(require "request-handlers.rkt")
(provide (all-defined-out))

;; server thread
(define (serve port-no)
  (displayln (~a "Server listening at " port-no))
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (thread
   (lambda ()
     (handle-incoming-connection in out)
     (close-ports in out))))

(define (handle-incoming-connection in out)
  ;; check message type
  (define message-type (deserialize (read in)))
  (cond [(equal? message-type CONN_REQ) (new-connection-handler in out)]
        [(equal? message-type NEW_PEER_ADDED) (add-new-peer in out)]
        [(equal? message-type PEER_DISCON) (remove-peer in out)]
        [(equal? message-type NEW_TXN) (process-transaction in out)]
        [(equal? message-type INIT_BLOCKCHAIN) (record-new-peer in out)]
        [(equal? message-type UPDATE_BLOCKCHAIN) (update-blockchain in out)]
        [(equal? message-type SYNC_BLOCKCHAIN) (reconnect-to-peer in out)]
        [else (displayln "bad message from client")]))
