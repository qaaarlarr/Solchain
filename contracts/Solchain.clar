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

(define-constant min-proposal-stake u10000000)
(define-constant voting-period u432)
(define-constant execution-delay u144)
(define-constant quorum-threshold u20)

(define-data-var proposal-count uint u0)
(define-data-var governance-active bool true)

(define-map proposals
    uint
    {
        creator: principal,
        title: (string-ascii 50),
        description: (string-ascii 200),
        proposal-type: (string-ascii 20),
        target-value: uint,
        votes-for: uint,
        votes-against: uint,
        total-voters: uint,
        status: (string-ascii 20),
        created-at: uint,
        voting-ends: uint,
        execution-ready: uint,
        executed: bool
    }
)

(define-map proposal-votes
    { proposal-id: uint, voter: principal }
    {
        vote: bool,
        weight: uint,
        timestamp: uint
    }
)

(define-map voter-participation
    principal
    {
        proposals-voted: uint,
        total-weight-cast: uint,
        last-vote-block: uint
    }
)

(define-map governance-parameters
    (string-ascii 30)
    uint
)

(define-private (initialize-governance)
    (begin
        (map-set governance-parameters "min-investment" min-investment)
        (map-set governance-parameters "funding-period" funding-period)
        (map-set governance-parameters "success-threshold" success-threshold)
        (map-set governance-parameters "max-projects" max-projects)
        true
    )
)

(define-public (create-proposal (title (string-ascii 50)) (description (string-ascii 200)) (proposal-type (string-ascii 20)) (target-value uint))
    (let (
        (proposal-id (var-get proposal-count))
        (voter-balance (ft-get-balance soltoken tx-sender))
    )
        (asserts! (var-get governance-active) (err u50))
        (asserts! (>= voter-balance min-proposal-stake) (err u51))
        (asserts! (or (is-eq proposal-type "parameter") 
                      (is-eq proposal-type "emergency") 
                      (is-eq proposal-type "upgrade")) (err u52))
        
        (map-set proposals proposal-id {
            creator: tx-sender,
            title: title,
            description: description,
            proposal-type: proposal-type,
            target-value: target-value,
            votes-for: u0,
            votes-against: u0,
            total-voters: u0,
            status: "active",
            created-at: stacks-block-height,
            voting-ends: (+ stacks-block-height voting-period),
            execution-ready: (+ stacks-block-height voting-period execution-delay),
            executed: false
        })
        
        (var-set proposal-count (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (cast-vote (proposal-id uint) (vote bool))
    (let (
        (proposal-data (unwrap! (map-get? proposals proposal-id) (err u53)))
        (voter-balance (ft-get-balance soltoken tx-sender))
        (existing-vote (map-get? proposal-votes {proposal-id: proposal-id, voter: tx-sender}))
        (current-participation (default-to 
            {proposals-voted: u0, total-weight-cast: u0, last-vote-block: u0}
            (map-get? voter-participation tx-sender)))
    )
        (asserts! (var-get governance-active) (err u54))
        (asserts! (is-eq (get status proposal-data) "active") (err u55))
        (asserts! (< stacks-block-height (get voting-ends proposal-data)) (err u56))
        (asserts! (> voter-balance u0) (err u57))
        (asserts! (is-none existing-vote) (err u58))
        
        (map-set proposal-votes
            {proposal-id: proposal-id, voter: tx-sender}
            {
                vote: vote,
                weight: voter-balance,
                timestamp: stacks-block-height
            }
        )
        
        (map-set proposals proposal-id
            (merge proposal-data {
                votes-for: (if vote 
                    (+ (get votes-for proposal-data) voter-balance)
                    (get votes-for proposal-data)),
                votes-against: (if vote
                    (get votes-against proposal-data)
                    (+ (get votes-against proposal-data) voter-balance)),
                total-voters: (+ (get total-voters proposal-data) u1)
            })
        )
        
        (map-set voter-participation tx-sender
            (merge current-participation {
                proposals-voted: (+ (get proposals-voted current-participation) u1),
                total-weight-cast: (+ (get total-weight-cast current-participation) voter-balance),
                last-vote-block: stacks-block-height
            })
        )
        
        (ok true)
    )
)

(define-public (finalize-proposal (proposal-id uint))
    (let ((proposal-data (unwrap! (map-get? proposals proposal-id) (err u59))))
        (asserts! (var-get governance-active) (err u60))
        (asserts! (is-eq (get status proposal-data) "active") (err u61))
        (asserts! (>= stacks-block-height (get voting-ends proposal-data)) (err u62))
        
        (let (
            (total-votes (+ (get votes-for proposal-data) (get votes-against proposal-data)))
            (total-supply (ft-get-supply soltoken))
            (quorum-met (>= (* total-votes u100) (* total-supply quorum-threshold)))
            (proposal-passed (> (get votes-for proposal-data) (get votes-against proposal-data)))
        )
            (if (and quorum-met proposal-passed)
                (map-set proposals proposal-id (merge proposal-data {status: "approved"}))
                (map-set proposals proposal-id (merge proposal-data {status: "rejected"}))
            )
            (ok true)
        )
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal-data (unwrap! (map-get? proposals proposal-id) (err u63))))
        (asserts! (var-get governance-active) (err u64))
        (asserts! (is-eq (get status proposal-data) "approved") (err u65))
        (asserts! (>= stacks-block-height (get execution-ready proposal-data)) (err u66))
        (asserts! (not (get executed proposal-data)) (err u67))
        
        (if (is-eq (get proposal-type proposal-data) "parameter")
            (begin
                (map-set governance-parameters "min-investment" (get target-value proposal-data))
                (map-set proposals proposal-id (merge proposal-data {executed: true}))
                (ok true)
            )
            (if (is-eq (get proposal-type proposal-data) "emergency")
                (begin
                    (var-set governance-active false)
                    (map-set proposals proposal-id (merge proposal-data {executed: true}))
                    (ok true)
                )
                (begin
                    (map-set proposals proposal-id (merge proposal-data {executed: true}))
                    (ok true)
                )
            )
        )
    )
)

(define-public (delegate-voting-power (delegate principal) (amount uint))
    (let (
        (voter-balance (ft-get-balance soltoken tx-sender))
        (current-delegation (default-to u0 (map-get? investments {project-id: u999999, investor: delegate})))
    )
        (asserts! (var-get governance-active) (err u68))
        (asserts! (<= amount voter-balance) (err u69))
        (asserts! (not (is-eq tx-sender delegate)) (err u70))
        
        (map-set investments 
            {project-id: u999999, investor: delegate}
            (+ current-delegation amount)
        )
        (ok true)
    )
)

(define-public (revoke-delegation (delegate principal) (amount uint))
    (let ((current-delegation (default-to u0 (map-get? investments {project-id: u999999, investor: delegate}))))
        (asserts! (var-get governance-active) (err u71))
        (asserts! (<= amount current-delegation) (err u72))
        
        (if (is-eq amount current-delegation)
            (map-delete investments {project-id: u999999, investor: delegate})
            (map-set investments 
                {project-id: u999999, investor: delegate}
                (- current-delegation amount))
        )
        (ok true)
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (ok (map-get? proposals proposal-id))
)

(define-read-only (get-proposal-vote (proposal-id uint) (voter principal))
    (ok (map-get? proposal-votes {proposal-id: proposal-id, voter: voter}))
)

(define-read-only (get-voter-participation (voter principal))
    (ok (map-get? voter-participation voter))
)

(define-read-only (get-governance-parameter (param-name (string-ascii 30)))
    (ok (map-get? governance-parameters param-name))
)

(define-read-only (get-total-proposals)
    (ok (var-get proposal-count))
)

(define-read-only (get-governance-status)
    (ok (var-get governance-active))
)

(define-read-only (get-voting-power (voter principal))
    (let (
        (direct-balance (ft-get-balance soltoken voter))
        (delegated-power (default-to u0 (map-get? investments {project-id: u999999, investor: voter})))
    )
        (ok (+ direct-balance delegated-power))
    )
)

(define-read-only (calculate-proposal-outcome (proposal-id uint))
    (let ((proposal-data (unwrap! (map-get? proposals proposal-id) (err u73))))
        (let (
            (total-votes (+ (get votes-for proposal-data) (get votes-against proposal-data)))
            (total-supply (ft-get-supply soltoken))
            (quorum-percentage (if (> total-supply u0) (/ (* total-votes u100) total-supply) u0))
        )
            (ok {
                quorum-met: (>= quorum-percentage quorum-threshold),
                votes-for: (get votes-for proposal-data),
                votes-against: (get votes-against proposal-data),
                quorum-percentage: quorum-percentage,
                would-pass: (> (get votes-for proposal-data) (get votes-against proposal-data))
            })
        )
    )
)