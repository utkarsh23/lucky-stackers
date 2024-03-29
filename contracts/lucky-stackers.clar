;; lucky-stackers
;; lucky stackers game

;; constants
;; =========
;; insufficient amount error
(define-constant ERR-INSUFFICIENT-AMT u500)
;; bad request
(define-constant ERR-BAD-REQUEST u400)
;; not a game participant, hence unauthorized
(define-constant ERR-NOT-PARTICIPANT u401)
;; store contract of contract creator
(define-constant CONTRACT-CREATOR tx-sender)
;; store min. game amount = 10 STX or 10^7 micro-STX
(define-constant MIN-GAME-AMOUNT u10000000)
;; 5 days in seconds
(define-constant FIVE-DAYS u432000)
;; Random number generation
;; Credits: https://github.com/FriendsFerdinand/random-test
(define-constant BUFF_TO_BYTE (list
    0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a 0x0b 0x0c 0x0d 0x0e 0x0f
    0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f
    0x20 0x21 0x22 0x23 0x24 0x25 0x26 0x27 0x28 0x29 0x2a 0x2b 0x2c 0x2d 0x2e 0x2f
    0x30 0x31 0x32 0x33 0x34 0x35 0x36 0x37 0x38 0x39 0x3a 0x3b 0x3c 0x3d 0x3e 0x3f
    0x40 0x41 0x42 0x43 0x44 0x45 0x46 0x47 0x48 0x49 0x4a 0x4b 0x4c 0x4d 0x4e 0x4f
    0x50 0x51 0x52 0x53 0x54 0x55 0x56 0x57 0x58 0x59 0x5a 0x5b 0x5c 0x5d 0x5e 0x5f
    0x60 0x61 0x62 0x63 0x64 0x65 0x66 0x67 0x68 0x69 0x6a 0x6b 0x6c 0x6d 0x6e 0x6f
    0x70 0x71 0x72 0x73 0x74 0x75 0x76 0x77 0x78 0x79 0x7a 0x7b 0x7c 0x7d 0x7e 0x7f
    0x80 0x81 0x82 0x83 0x84 0x85 0x86 0x87 0x88 0x89 0x8a 0x8b 0x8c 0x8d 0x8e 0x8f
    0x90 0x91 0x92 0x93 0x94 0x95 0x96 0x97 0x98 0x99 0x9a 0x9b 0x9c 0x9d 0x9e 0x9f
    0xa0 0xa1 0xa2 0xa3 0xa4 0xa5 0xa6 0xa7 0xa8 0xa9 0xaa 0xab 0xac 0xad 0xae 0xaf
    0xb0 0xb1 0xb2 0xb3 0xb4 0xb5 0xb6 0xb7 0xb8 0xb9 0xba 0xbb 0xbc 0xbd 0xbe 0xbf
    0xc0 0xc1 0xc2 0xc3 0xc4 0xc5 0xc6 0xc7 0xc8 0xc9 0xca 0xcb 0xcc 0xcd 0xce 0xcf
    0xd0 0xd1 0xd2 0xd3 0xd4 0xd5 0xd6 0xd7 0xd8 0xd9 0xda 0xdb 0xdc 0xdd 0xde 0xdf
    0xe0 0xe1 0xe2 0xe3 0xe4 0xe5 0xe6 0xe7 0xe8 0xe9 0xea 0xeb 0xec 0xed 0xee 0xef
    0xf0 0xf1 0xf2 0xf3 0xf4 0xf5 0xf6 0xf7 0xf8 0xf9 0xfa 0xfb 0xfc 0xfd 0xfe 0xff
))

;; data maps and vars
;; ==================
;; game ID
(define-data-var game-id uint u0)
;; any participant
(define-data-var any-participant principal CONTRACT-CREATOR)
;; any amount
(define-data-var any-amount uint u0)
;; games data
;; (tuple (name "blockstack") (id 1337)) --> using tuple
;; {name: "blockstack", id: 1337} --> using curly brackets (a shorthand for tuple)
(define-map game-data { id: uint } { time: uint, amount: uint, pending: bool, players: (list 6 principal), winner: (optional principal) })

;; private functions

;; Fetch random value from 0 to 5
(define-read-only (get-random-val)
    (let ((seed (unwrap-panic (get-block-info? vrf-seed (- block-height u1)))))
        (mod (unwrap-panic (index-of BUFF_TO_BYTE (unwrap-panic (element-at seed u0)))) u6)
    )
)

(define-private (add-player-to-game (sender-addr principal) (joining-game-id uint))
    (let (
        (game (unwrap-panic (map-get? game-data { id: joining-game-id })))
    )
    (map-set game-data { id: joining-game-id }
        { time: (get time game), amount: (get amount game), pending: true, winner: none,
            players: (unwrap-panic (as-max-len? (append (get players game) sender-addr) u6)) })
))

(define-private (complete-game (joining-game-id uint))
    (let (
        (game (unwrap-panic (map-get? game-data { id: joining-game-id })))
    )
    (if (is-eq (len (get players game)) u6)
        (begin
            (let (
                (total-amount (* (get amount game) u6))
                (commission (* u2 (/ total-amount u100)))
                (winner-prize (- total-amount commission))
                (winner (get-random-val))
            )
            (as-contract (unwrap-panic (stx-transfer? commission (as-contract tx-sender) CONTRACT-CREATOR)))
            (as-contract (unwrap-panic (stx-transfer? winner-prize (as-contract tx-sender)
                (unwrap-panic (element-at (get players game) winner)))))
            (map-set game-data { id: joining-game-id }
                { time: (get time game), amount: (get amount game), winner: (element-at (get players game) winner),
                    pending: false, players: (get players game) })
            )
            (ok true)
        )
        (ok false)
    )
))

(define-private (abort-disburse (seq-el principal) (initial-b principal))
    (begin
        (unwrap-panic (stx-transfer? (var-get any-amount) initial-b seq-el))
        (unwrap-panic (some initial-b))
    )
)

(define-private (abort-disburse-stx (aborting-game-id uint))
    (let (
        (game (unwrap-panic (map-get? game-data { id: aborting-game-id })))
    )
        (var-set any-amount (get amount game))
        (fold abort-disburse (get players game) (as-contract tx-sender))
    )
)

(define-private (is-not-participant (player principal))
    (not (is-eq player (var-get any-participant)))
)

(define-private (check-if-participant (any-game-id uint) (sender principal))
    (let (
        (game (unwrap-panic (map-get? game-data { id: any-game-id })))
    )
        (var-set any-participant sender)
        (if (< (len (filter is-not-participant (get players game))) (len (get players game)))
            (unwrap-panic (ok true))
            (unwrap-panic (ok false))
        )
    )
)

;; public functions
(define-read-only (get-last-game-id)
    (var-get game-id))

(define-read-only (get-game-data (read-game-id uint))
    (unwrap-panic (map-get? game-data { id: read-game-id })))

(define-public (is-game-pending (read-game-id uint))
    (let (
        (game (unwrap-panic (map-get? game-data { id: read-game-id })))
    )
        (ok (get pending game))
    )
)

(define-public (abort-game (aborting-game-id uint))
    (let (
        (game (unwrap-panic (map-get? game-data { id: aborting-game-id })))
        (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
        (if
            (and
                (check-if-participant aborting-game-id tx-sender)
                (get pending game)
                (>= (- current-time (get time game)) FIVE-DAYS)
            )
            (begin
                (abort-disburse-stx aborting-game-id)
                (map-set game-data { id: aborting-game-id }
                    { time: (get time game), amount: (get amount game), winner: none,
                        pending: false, players: (get players game) })
                (ok true)
            )
            (err ERR-NOT-PARTICIPANT)
        )
    )
)

(define-public (join-game (joining-game-id uint))
    (let (
        (game (unwrap-panic (map-get? game-data { id: joining-game-id })))
    )
        (if (and (get pending game) (not (check-if-participant joining-game-id tx-sender)))
            (match (stx-transfer? (get amount game) tx-sender (as-contract tx-sender))
                success (begin
                    (add-player-to-game tx-sender joining-game-id)
                    (unwrap-panic (complete-game joining-game-id))
                    (ok true)
                )
                error (err ERR-INSUFFICIENT-AMT)
            )
            (err ERR-INSUFFICIENT-AMT)
        )
    )
)

(define-public (create-game (game-amount uint))
    (if (>= game-amount MIN-GAME-AMOUNT)
        (let (
            (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        )
            (match (stx-transfer? game-amount tx-sender (as-contract tx-sender))
                success (begin
                    (var-set game-id (+ (var-get game-id) u1))
                    (map-set game-data { id: (var-get game-id) }
                        { time: current-time, amount: game-amount, pending: true,
                            players: (list tx-sender), winner: none })
                    (ok true)
                )
                error (err ERR-INSUFFICIENT-AMT)
            )
        )
        (err ERR-INSUFFICIENT-AMT)
    )
)
