#lang racket

(provide (all-defined-out))

(require racket/serialize)

(struct peer (port
              connected-peers
              blockchain
              balance)
  #:prefab)

(define localhost "127.0.0.1")
(define INIT-BALANCE 50)
(define ZERO 0)
(define CONN_REQ 0)
(define NEW_PEER_ADDED 1)
(define BLOCKCHAIN_BROADCAST 2)
(define PEER_DISCON 3)
(define SYNC_BLOCKCHAIN 4)
(define NEW_TXN 5)
(define INIT_BLOCKCHAIN 6)
(define UPDATE_BLOCKCHAIN 7)

(define RECONNECTING #t)
(define NOT_RECONNECTING #f)

(define updated-peers-channel (make-channel))
(define get-current-peer-struct-channel (make-channel))
(define get-current-blockchain-channel (make-channel))
(define update-blockchain-channel (make-channel))
(define get-current-peers-channel (make-channel))
(define reply-channel (make-channel))
(define repeat-channel (make-channel))


;; closes both in and out ports
(define (close-ports in out)
  (close-input-port in)
  (close-output-port out))

;; prints set of peers
(define (print-peers peers-set)
  (cond [(set-empty? peers-set) (displayln "You are not connected to any peers.")]
        [else
         (for ([p peers-set])
           (display (~a p " ")))
         (display "\n")]))



;; broadcast-data takes the list of peers and the data to broadcast
(define (broadcast-data recipients message-type data)
  (displayln "Broadcast initiated...")
  (for ([peer-port recipients])
    (define-values (in out) (tcp-connect localhost peer-port))
    ;; send message type so server knows what to do next
    (write (serialize message-type) out)
    (flush-output out)
    ;; send newly added peer and its peers (data)
    (write (serialize data) out)
    (flush-output out)
    (close-ports in out))
  (displayln "Broadcast done!"))


;; fetch the set of current peers
(define (get-current-peers)
  (channel-put get-current-peers-channel reply-channel)
  (channel-get reply-channel))

;; fetch current blockchain
(define (get-current-blockchain)
  (channel-put get-current-blockchain-channel reply-channel)
  (channel-get reply-channel))