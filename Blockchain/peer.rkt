#lang racket

(require racket/serialize)
(require "network-constants.rkt")
(require "server.rkt")
(require "client-side-handlers.rkt")
(require "data-structures.rkt")
(require "blockchain.rkt")


(define argv (current-command-line-arguments))
(define (main)
  ;; set the port-no of this peer
  (cond [(not (= (vector-length argv) 1)) (begin0
                                            (displayln "usage: <peer> port-no")
                                            (exit))]
        [else (letrec ([current-port (string->number (vector-ref argv 0))]
                    [current-peer (load-peer-from-disk current-port)])
                (displayln (~a "Peer running at port# " current-port))
                ;; start tcp server
                (serve current-port)
                ;; start the loop that always has the latest peer information
                (thread (lambda () (loop current-peer)))
                ;; start accepting user commands
                (handle-user-commands))]))


;; always has the latest peer information
;; other threads will communicate with this thread if any changes are made
(define (loop current-peer)
  (sync (handle-evt updated-peers-channel
                    (lambda (peers)
                      (loop (peer (peer-port current-peer)
                                  peers
                                  (peer-blockchain current-peer)
                                  (peer-balance current-peer)))))
        (handle-evt update-blockchain-channel
                    (lambda (blockchain)
                      (loop (peer (peer-port current-peer)
                                  (peer-connected-peers current-peer)
                                  blockchain
                                  (peer-balance current-peer)))))
        (handle-evt get-current-blockchain-channel
                    (lambda (reply-channel)
                      (channel-put reply-channel
                                   (peer-blockchain current-peer))
                      (loop current-peer)))
        (handle-evt get-current-peers-channel
                    (lambda (reply-channel)
                      (channel-put reply-channel
                                   (peer-connected-peers
                                    current-peer))
                      (loop current-peer)))
        (handle-evt get-current-peer-struct-channel
                    (lambda (reply-current-struct-channel)
                      (channel-put reply-current-struct-channel
                                   current-peer)
                      (loop current-peer)))
        (handle-evt repeat-channel
                    (lambda (dummy)
                      (loop current-peer)))))


;; handle user's commands
(define reply-peer-struct-channel (make-channel))
(define (handle-user-commands)
  (display ">>> ")
  (define command (read-line (current-input-port)))
  (channel-put get-current-peer-struct-channel reply-peer-struct-channel)
  (define current-peer (channel-get reply-peer-struct-channel))
  (cond [(equal? command "show peers") (print-peers (peer-connected-peers current-peer))]
        [(equal? command "show blockchain") (print-blockchain (peer-blockchain current-peer))]
        [(regexp-match #px"connect \\d+" command) (let* ([words (string-split command)]
                                                         [other-peer-port (string->number (second words))]
                                                         [current-blockchain (peer-blockchain current-peer)])
                                                    (cond [(= (peer-port current-peer) other-peer-port) (displayln "You cannot connect to yourself!") ]
                                                          [(= (length (chain-blocks current-blockchain)) 1)
                                                           (connect-to-peer NOT_RECONNECTING current-peer other-peer-port)]
                                                          [else (connect-to-peer RECONNECTING current-peer other-peer-port)]))]
        [(regexp-match #px"send \\d+ coins to \\d+" command) (let* ([words (string-split command)]
                                                                    [coins (string->number (second words))]
                                                                    [recipient-port (string->number (last words))])
                                                               (cond [(equal? recipient-port (peer-port current-peer))
                                                                      (displayln (~a "You cannot send coins to yourself!"))]
                                                                     [else (make-transaction current-peer
                                                                                 recipient-port
                                                                                 coins)]))]
        [(equal? command "stop") (begin0
                                   (displayln "Notifying peers that I'm about to disconnect...")
                                   (broadcast-data (peer-connected-peers current-peer)
                                                   PEER_DISCON
                                                   (peer-port current-peer))
                                   (displayln "Terminating this peer...")
                                   (save-to-disk current-peer)
                                   (exit))]
        [else (displayln "Invalid command!")])
  (channel-put repeat-channel "repeat")
  (handle-user-commands))


;; saves data passed to disk in file port-no.data
(define (save-to-disk current-peer)
  (define port-no (peer-port current-peer))
  ;; clearning peer set so everything works as expected when loaded back from disk
  (define current-peer-to-save (peer port-no
                                     (set )
                                     (peer-blockchain current-peer)
                                     (peer-balance current-peer)))
  (define filename (string-append-immutable (number->string port-no)
                                            ".data"))
  (let ([out-port (open-output-file filename #:exists 'replace)])
    (write (serialize current-peer-to-save) out-port)
    (close-output-port out-port)))

;; loads peer struct from port-no.data if file exists, otherwise returns a new peer
(define (load-peer-from-disk port-no)
  (define filename (string-append-immutable (number->string port-no)
                                            ".data"))
  (cond [(file-exists? filename) (letrec ([in-port (open-input-file filename)]
                                     [current-peer (deserialize (read in-port))])
                                 (close-input-port in-port)
                                 current-peer)]
          [else (peer port-no
                      (set )
                      (chain (list (mine-block (set-hash-of-block (init-block port-no)) 0)))
                      INIT-BALANCE)]))

;; start peer
(main)