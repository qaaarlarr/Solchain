(define-constant contract-owner tx-sender)
(define-constant min-investment u1000000)
(define-constant max-projects u100)
(define-constant funding-period u144)
(define-constant success-threshold u800000000)

(define-fungible-token soltoken)

(define-non-fungible-token project uint)

(define-map projects 
    uint 
    {
        owner: principal,
        target: uint,
        raised: uint,
        status: (string-ascii 20),
        investors: uint,
        start-block: uint
    }
)

(define-map investments
    { project-id: uint, investor: principal }
    uint
)

(define-data-var project-count uint u0)
(define-data-var total-funds-raised uint u0)

(define-public (create-project (target uint))
    (let ((project-id (var-get project-count)))
        (asserts! (< project-id max-projects) (err u1))
        (asserts! (> target min-investment) (err u2))
        (try! (nft-mint? project project-id tx-sender))
        (map-set projects project-id {
            owner: tx-sender,
            target: target,
            raised: u0,
            status: "active",
            investors: u0,
            start-block: stacks-block-height
        })
        (var-set project-count (+ project-id u1))
        (ok project-id)
    )
)

(define-public (invest (project-id uint) (amount uint))
    (let (
        (project-data (unwrap! (map-get? projects project-id) (err u3)))
        (current-investment (default-to u0 (map-get? investments {project-id: project-id, investor: tx-sender})))
    )
        (asserts! (is-eq (get status project-data) "active") (err u4))
        (asserts! (>= amount min-investment) (err u5))
        (asserts! (<= (+ (get raised project-data) amount) (get target project-data)) (err u6))
        
        (try! (stx-transfer? amount tx-sender contract-owner))
        (try! (ft-mint? soltoken amount tx-sender))
        
        (map-set projects project-id (merge project-data {
            raised: (+ (get raised project-data) amount),
            investors: (+ (get investors project-data) u1)
        }))
        
        (map-set investments 
            {project-id: project-id, investor: tx-sender}
            (+ current-investment amount)
        )
        
        (var-set total-funds-raised (+ (var-get total-funds-raised) amount))
        (ok true)
    )
)

(define-public (finalize-project (project-id uint))
    (let ((project-data (unwrap! (map-get? projects project-id) (err u7))))
        (asserts! (is-eq (get owner project-data) tx-sender) (err u8))
        (asserts! (is-eq (get status project-data) "active") (err u9))
        (asserts! (>= (- stacks-block-height (get start-block project-data)) funding-period) (err u10))
        
        (if (>= (get raised project-data) (get target project-data))
            (map-set projects project-id (merge project-data {status: "success"}))
            (map-set projects project-id (merge project-data {status: "failed"}))
        )
        (ok true)
    )
)

(define-public (claim-refund (project-id uint))
    (let (
        (project-data (unwrap! (map-get? projects project-id) (err u11)))
        (investment (unwrap! (map-get? investments {project-id: project-id, investor: tx-sender}) (err u12)))
    )
        (asserts! (is-eq (get status project-data) "failed") (err u13))
        (try! (stx-transfer? investment contract-owner tx-sender))
        (try! (ft-burn? soltoken investment tx-sender))
        (map-delete investments {project-id: project-id, investor: tx-sender})
        (ok true)
    )
)

(define-read-only (get-project (project-id uint))
    (ok (map-get? projects project-id))
)

(define-read-only (get-investment (project-id uint) (investor principal))
    (ok (map-get? investments {project-id: project-id, investor: investor}))
)

(define-read-only (get-total-projects)
    (ok (var-get project-count))
)

(define-read-only (get-total-funds)
    (ok (var-get total-funds-raised))
)