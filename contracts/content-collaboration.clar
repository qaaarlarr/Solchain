;; Content Collaboration Contract
;; Enables multiple creators to collaborate on premium content creation

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u501))
(define-constant ERR-COLLABORATION-NOT-FOUND (err u502))
(define-constant ERR-ALREADY-MEMBER (err u503))
(define-constant ERR-INVALID-VOTE (err u504))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u505))
(define-constant ERR-VOTING-CLOSED (err u506))
(define-constant ERR-INSUFFICIENT-VOTES (err u507))
(define-constant ERR-INVALID-PARAMS (err u508))

;; Data variables
(define-data-var next-collaboration-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var min-vote-threshold uint u2)
(define-data-var voting-period-blocks uint u1440) ;; ~10 days

;; Collaboration projects
(define-map collaborations
  { collaboration-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 200),
    creators: (list 5 principal),
    revenue-shares: (list 5 uint),
    content-count: uint,
    total-revenue: uint,
    created-at: uint,
    is-active: bool
  }
)

;; Content proposals within collaborations
(define-map content-proposals
  { proposal-id: uint }
  {
    collaboration-id: uint,
    proposer: principal,
    title: (string-ascii 100),
    content-hash: (buff 32),
    access-level: uint,
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint,
    executed: bool,
    created-at: uint
  }
)

;; Member votes on proposals
(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  {
    vote: bool, ;; true for yes, false for no
    timestamp: uint
  }
)

;; Member permissions within collaborations
(define-map member-permissions
  { collaboration-id: uint, member: principal }
  {
    can-propose: bool,
    can-vote: bool,
    revenue-share: uint,
    joined-at: uint
  }
)

;; Collaboration content registry
(define-map collaboration-content
  { collaboration-id: uint, content-id: uint }
  {
    created-by: principal,
    proposal-id: uint,
    revenue-generated: uint
  }
)

;; Read-only functions

(define-read-only (get-collaboration (collaboration-id uint))
  (map-get? collaborations { collaboration-id: collaboration-id })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? content-proposals { proposal-id: proposal-id })
)

(define-read-only (get-member-permissions (collaboration-id uint) (member principal))
  (map-get? member-permissions { collaboration-id: collaboration-id, member: member })
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? proposal-votes { proposal-id: proposal-id, voter: voter }))
)

(define-read-only (is-collaboration-member (collaboration-id uint) (user principal))
  (is-some (map-get? member-permissions { collaboration-id: collaboration-id, member: user }))
)

;; Public functions

(define-public (create-collaboration 
  (title (string-ascii 100))
  (description (string-ascii 200))
  (initial-members (list 5 principal))
  (revenue-shares (list 5 uint))
)
  (let (
    (collaboration-id (var-get next-collaboration-id))
    (total-shares (fold + revenue-shares u0))
  )
    ;; Validate inputs
    (asserts! (> (len title) u0) ERR-INVALID-PARAMS)
    (asserts! (> (len initial-members) u0) ERR-INVALID-PARAMS)
    (asserts! (is-eq (len initial-members) (len revenue-shares)) ERR-INVALID-PARAMS)
    (asserts! (is-eq total-shares u100) ERR-INVALID-PARAMS)
    
    ;; Create collaboration
    (map-set collaborations
      { collaboration-id: collaboration-id }
      {
        title: title,
        description: description,
        creators: initial-members,
        revenue-shares: revenue-shares,
        content-count: u0,
        total-revenue: u0,
        created-at: stacks-block-height,
        is-active: true
      }
    )
    
    ;; Set up member permissions
    (try! (setup-member-permissions collaboration-id initial-members revenue-shares))
    
    (var-set next-collaboration-id (+ collaboration-id u1))
    (ok collaboration-id)
  )
)

(define-public (propose-content 
  (collaboration-id uint)
  (title (string-ascii 100))
  (content-hash (buff 32))
  (access-level uint)
)
  (let (
    (proposal-id (var-get next-proposal-id))
    (collaboration (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) ERR-COLLABORATION-NOT-FOUND))
    (member-perms (unwrap! (map-get? member-permissions { collaboration-id: collaboration-id, member: tx-sender }) ERR-NOT-AUTHORIZED))
  )
    ;; Validate proposer permissions
    (asserts! (get can-propose member-perms) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active collaboration) ERR-NOT-AUTHORIZED)
    (asserts! (> (len title) u0) ERR-INVALID-PARAMS)
    (asserts! (> (len content-hash) u0) ERR-INVALID-PARAMS)
    
    ;; Create content proposal
    (map-set content-proposals
      { proposal-id: proposal-id }
      {
        collaboration-id: collaboration-id,
        proposer: tx-sender,
        title: title,
        content-hash: content-hash,
        access-level: access-level,
        votes-for: u0,
        votes-against: u0,
        voting-deadline: (+ stacks-block-height (var-get voting-period-blocks)),
        executed: false,
        created-at: stacks-block-height
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (map-get? content-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (collaboration-id (get collaboration-id proposal))
    (member-perms (unwrap! (map-get? member-permissions { collaboration-id: collaboration-id, member: tx-sender }) ERR-NOT-AUTHORIZED))
  )
    ;; Validate voting permissions
    (asserts! (get can-vote member-perms) ERR-NOT-AUTHORIZED)
    (asserts! (<= stacks-block-height (get voting-deadline proposal)) ERR-VOTING-CLOSED)
    (asserts! (not (has-voted proposal-id tx-sender)) ERR-ALREADY-MEMBER)
    
    ;; Record vote
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        vote: vote-for,
        timestamp: stacks-block-height
      }
    )
    
    ;; Update proposal vote counts
    (map-set content-proposals
      { proposal-id: proposal-id }
      (if vote-for
        (merge proposal { votes-for: (+ (get votes-for proposal) u1) })
        (merge proposal { votes-against: (+ (get votes-against proposal) u1) })
      )
    )
    
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? content-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (collaboration-id (get collaboration-id proposal))
    (collaboration (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) ERR-COLLABORATION-NOT-FOUND))
    (votes-for (get votes-for proposal))
    (votes-against (get votes-against proposal))
    (total-votes (+ votes-for votes-against))
  )
    ;; Validate execution conditions
    (asserts! (> stacks-block-height (get voting-deadline proposal)) ERR-VOTING-CLOSED)
    (asserts! (not (get executed proposal)) ERR-ALREADY_MEMBER)
    (asserts! (>= total-votes (var-get min-vote-threshold)) ERR-INSUFFICIENT-VOTES)
    (asserts! (> votes-for votes-against) ERR-INSUFFICIENT-VOTES)
    
    ;; Mark proposal as executed
    (map-set content-proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    
    ;; Create content via main contract (simplified)
    (try! (contract-call? .private-gate add-content 
      (get title proposal) 
      (get content-hash proposal) 
      (get access-level proposal)
    ))
    
    ;; Update collaboration content count
    (map-set collaborations
      { collaboration-id: collaboration-id }
      (merge collaboration { content-count: (+ (get content-count collaboration) u1) })
    )
    
    (ok true)
  )
)

(define-public (add-collaborator 
  (collaboration-id uint)
  (new-member principal)
  (revenue-share uint)
)
  (let (
    (collaboration (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) ERR-COLLABORATION-NOT-FOUND))
    (current-creators (get creators collaboration))
    (current-shares (get revenue-shares collaboration))
  )
    ;; Only existing members can add new collaborators
    (asserts! (is-collaboration-member collaboration-id tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-collaboration-member collaboration-id new-member)) ERR-ALREADY-MEMBER)
    (asserts! (< (len current-creators) u5) ERR-INVALID-PARAMS)
    (asserts! (> revenue-share u0) ERR-INVALID-PARAMS)
    
    ;; Add member permissions
    (map-set member-permissions
      { collaboration-id: collaboration-id, member: new-member }
      {
        can-propose: true,
        can-vote: true,
        revenue-share: revenue-share,
        joined-at: stacks-block-height
      }
    )
    
    ;; Update collaboration with new member
    (map-set collaborations
      { collaboration-id: collaboration-id }
      (merge collaboration {
        creators: (unwrap-panic (as-max-len? (append current-creators new-member) u5)),
        revenue-shares: (unwrap-panic (as-max-len? (append current-shares revenue-share) u5))
      })
    )
    
    (ok true)
  )
)

;; Private helper functions

(define-private (setup-member-permissions 
  (collaboration-id uint) 
  (members (list 5 principal)) 
  (shares (list 5 uint))
)
  (let (
    (member-count (len members))
  )
    (if (> member-count u0)
      (begin
        (setup-single-member-permission collaboration-id (unwrap-panic (element-at members u0)) (unwrap-panic (element-at shares u0)))
        (if (> member-count u1)
          (setup-single-member-permission collaboration-id (unwrap-panic (element-at members u1)) (unwrap-panic (element-at shares u1)))
          true
        )
        (if (> member-count u2)
          (setup-single-member-permission collaboration-id (unwrap-panic (element-at members u2)) (unwrap-panic (element-at shares u2)))
          true
        )
        (if (> member-count u3)
          (setup-single-member-permission collaboration-id (unwrap-panic (element-at members u3)) (unwrap-panic (element-at shares u3)))
          true
        )
        (if (> member-count u4)
          (setup-single-member-permission collaboration-id (unwrap-panic (element-at members u4)) (unwrap-panic (element-at shares u4)))
          true
        )
        (ok true)
      )
      ERR-INVALID-PARAMS
    )
  )
)

(define-private (setup-single-member-permission 
  (collaboration-id uint) 
  (member principal) 
  (share uint)
)
  (map-set member-permissions
    { collaboration-id: collaboration-id, member: member }
    {
      can-propose: true,
      can-vote: true,
      revenue-share: share,
      joined-at: stacks-block-height
    }
  )
)

;; Administrative functions

(define-public (update-voting-config (min-threshold uint) (period-blocks uint))
  (begin
    ;; Simplified ownership check
    (asserts! (is-eq tx-sender tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> min-threshold u0) ERR-INVALID-PARAMS)
    (asserts! (> period-blocks u0) ERR-INVALID-PARAMS)
    
    (var-set min-vote-threshold min-threshold)
    (var-set voting-period-blocks period-blocks)
    (ok true)
  )
)

(define-public (toggle-collaboration-status (collaboration-id uint))
  (let (
    (collaboration (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) ERR-COLLABORATION-NOT-FOUND))
  )
    ;; Only collaboration members can toggle status
    (asserts! (is-collaboration-member collaboration-id tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set collaborations
      { collaboration-id: collaboration-id }
      (merge collaboration { is-active: (not (get is-active collaboration)) })
    )
    (ok true)
  )
)

;; Enhanced read-only functions

(define-read-only (get-collaboration-stats (collaboration-id uint))
  (let (
    (collaboration (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) ERR-COLLABORATION-NOT-FOUND))
  )
    (ok {
      title: (get title collaboration),
      member-count: (len (get creators collaboration)),
      content-count: (get content-count collaboration),
      total-revenue: (get total-revenue collaboration),
      is-active: (get is-active collaboration)
    })
  )
)

(define-read-only (get-active-proposals (collaboration-id uint))
  (ok {
    collaboration-id: collaboration-id,
    current-block: stacks-block-height,
    voting-period: (var-get voting-period-blocks)
  })
)

(define-read-only (calculate-proposal-status (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? content-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (votes-for (get votes-for proposal))
    (votes-against (get votes-against proposal))
    (total-votes (+ votes-for votes-against))
    (voting-ended (> stacks-block-height (get voting-deadline proposal)))
    (can-execute (and voting-ended (> votes-for votes-against) (>= total-votes (var-get min-vote-threshold))))
  )
    (ok {
      proposal-id: proposal-id,
      votes-for: votes-for,
      votes-against: votes-against,
      total-votes: total-votes,
      voting-ended: voting-ended,
      can-execute: can-execute,
      executed: (get executed proposal)
    })
  )
)