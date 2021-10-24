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
(define-map game-data { id: uint } { time: uint, amount: uint, pending: bool, players: (list 6 principal) })

;; private functions
(define-private (add-player-to-game (sender-addr principal) (joining-game-id uint))
    (let (
        (game (unwrap-panic (map-get? game-data { id: joining-game-id })))
    )
    (map-set game-data { id: joining-game-id }
        { time: (get time game), amount: (get amount game), pending: true,
            players: (unwrap-panic (as-max-len? (append (get players game) sender-addr) u6)) })
))

(define-private (get-player (top-three-indexes (list 3 uint)) (players (list 6 principal)) (index uint))
    (unwrap-panic (element-at players (unwrap-panic (element-at top-three-indexes index))))
)

(define-private (complete-game (joining-game-id uint))
    (let (
        (game (unwrap-panic (map-get? game-data { id: joining-game-id })))
    )
    (if (is-eq (len (get players game)) u6)
        (begin
            (let (
                (total-amount (* (get amount game) u6))
                (commission (* u2 (/ total-amount u100)))
                (disbursement-amount (- total-amount commission))
                (first-place (* u50 (/ disbursement-amount u100)))
                (second-place (* u35 (/ disbursement-amount u100)))
                (third-place (* u15 (/ disbursement-amount u100)))
                (top-three-indexes (list u4 u2 u5))
            )
            (unwrap-panic (stx-transfer? commission (as-contract tx-sender) CONTRACT-CREATOR))
            (unwrap-panic (stx-transfer? first-place (as-contract tx-sender)
                (get-player top-three-indexes (get players game) u0)))
            (unwrap-panic (stx-transfer? second-place (as-contract tx-sender)
                (get-player top-three-indexes (get players game) u1)))
            (unwrap-panic (stx-transfer? third-place (as-contract tx-sender)
                (get-player top-three-indexes (get players game) u2)))
            )
            (map-set game-data { id: joining-game-id }
                { time: (get time game), amount: (get amount game),
                    pending: false, players: (get players game) })
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
        (if (is-eq (len (filter is-not-participant (get players game))) (- (len (get players game)) u1))
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
                    { time: (get time game), amount: (get amount game),
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
        (if (and (get pending game) (check-if-participant joining-game-id tx-sender))
            (match (stx-transfer? (get amount game) tx-sender (as-contract tx-sender))
                success (begin
                    (add-player-to-game tx-sender joining-game-id)
                    (complete-game joining-game-id)
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
                            players: (list tx-sender) })
                    (ok true)
                )
                error (err ERR-INSUFFICIENT-AMT)
            )
        )
        (err ERR-INSUFFICIENT-AMT)
    )
)
