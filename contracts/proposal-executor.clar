;; Proposal Execution Engine for VoteLock
;; Enables automatic execution of successful governance proposals

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u301))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u302))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u303))
(define-constant ERR-INVALID-ACTION-TYPE (err u304))
(define-constant ERR-INVALID-PARAMETERS (err u305))
(define-constant ERR-EXECUTION-FAILED (err u306))
(define-constant ERR-INSUFFICIENT-FUNDS (err u307))
(define-constant ERR-VOTING-STILL_ACTIVE (err u308))

;; Data variables
(define-data-var execution-admin principal tx-sender)
(define-data-var execution-delay uint u144) ;; 24 hour delay after proposal ends
(define-data-var min-participation-threshold uint u100) ;; Minimum voting power required
(define-data-var treasury-balance uint u0)

;; Executable action types
(define-map executable-actions
  { action-type: (string-ascii 30) }
  {
    description: (string-ascii 100),
    requires-amount: bool,
    requires-recipient: bool,
    requires-parameter: bool,
    min-voting-threshold: uint,
    enabled: bool
  }
)

;; Proposal execution records
(define-map proposal-executions
  { proposal-id: uint }
  {
    action-type: (string-ascii 30),
    parameters: {
      amount: (optional uint),
      recipient: (optional principal),
      parameter-name: (optional (string-ascii 30)),
      parameter-value: (optional uint)
    },
    executed-at: uint,
    executed-by: principal,
    execution-result: bool
  }
)

;; Pending proposal actions
(define-map pending-executions
  { proposal-id: uint }
  {
    action-type: (string-ascii 30),
    target-amount: (optional uint),
    target-recipient: (optional principal),
    parameter-update: (optional (string-ascii 30)),
    new-value: (optional uint),
    queued-at: uint,
    execution-eligible-at: uint
  }
)

;; Initialize executable action types
(define-private (initialize-action-types)
  (begin
    ;; Treasury transfer action
    (map-set executable-actions
      { action-type: "treasury-transfer" }
      {
        description: "Transfer funds from treasury to recipient",
        requires-amount: true,
        requires-recipient: true,
        requires-parameter: false,
        min-voting-threshold: u1000,
        enabled: true
      }
    )
    ;; Parameter update action
    (map-set executable-actions
      { action-type: "update-parameter" }
      {
        description: "Update system parameter value",
        requires-amount: false,
        requires-recipient: false,
        requires-parameter: true,
        min-voting-threshold: u500,
        enabled: true
      }
    )
    ;; Reward pool funding
    (map-set executable-actions
      { action-type: "fund-rewards" }
      {
        description: "Add funds to reward pool",
        requires-amount: true,
        requires-recipient: false,
        requires-parameter: false,
        min-voting-threshold: u300,
        enabled: true
      }
    )
    ;; Admin change action
    (map-set executable-actions
      { action-type: "change-admin" }
      {
        description: "Change system administrator",
        requires-amount: false,
        requires-recipient: true,
        requires-parameter: false,
        min-voting-threshold: u2000,
        enabled: true
      }
    )
    (ok true)
  )
)

;; Queue proposal for execution after voting ends
(define-public (queue-proposal-execution 
  (proposal-id uint)
  (action-type (string-ascii 30))
  (amount (optional uint))
  (recipient (optional principal))
  (parameter-name (optional (string-ascii 30)))
  (parameter-value (optional uint)))
  (let
    (
      (proposal (unwrap! (contract-call? .VoteLock get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
      (current-height stacks-block-height)
      (action-info (unwrap! (map-get? executable-actions { action-type: action-type }) ERR-INVALID-ACTION-TYPE))
      (proposal-status (unwrap! (contract-call? .VoteLock get-proposal-status proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
    ;; Validate proposal has ended and passed
    (asserts! (not (get active proposal-status)) ERR-VOTING-STILL_ACTIVE)
    (asserts! (get winning proposal-status) ERR-PROPOSAL-NOT-PASSED)
    (asserts! (>= (get total-votes proposal-status) (get min-voting-threshold action-info)) ERR-PROPOSAL-NOT-PASSED)
    (asserts! (get enabled action-info) ERR-INVALID-ACTION-TYPE)
    
    ;; Validate required parameters
    (asserts! (not (and (get requires-amount action-info) (is-none amount))) ERR-INVALID-PARAMETERS)
    (asserts! (not (and (get requires-recipient action-info) (is-none recipient))) ERR-INVALID-PARAMETERS)
    (asserts! (not (and (get requires-parameter action-info) (is-none parameter-name))) ERR-INVALID-PARAMETERS)
    
    ;; Queue execution
    (map-set pending-executions
      { proposal-id: proposal-id }
      {
        action-type: action-type,
        target-amount: amount,
        target-recipient: recipient,
        parameter-update: parameter-name,
        new-value: parameter-value,
        queued-at: current-height,
        execution-eligible-at: (+ current-height (var-get execution-delay))
      }
    )
    (ok true)
  )
)

;; Execute a queued proposal
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (pending (unwrap! (map-get? pending-executions { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (current-height stacks-block-height)
      (action-type (get action-type pending))
    )
    ;; Check execution timing
    (asserts! (>= current-height (get execution-eligible-at pending)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? proposal-executions { proposal-id: proposal-id })) ERR-PROPOSAL-ALREADY-EXECUTED)
    
    ;; Execute based on action type
    (let
      (
        (execution-result 
          (if (is-eq action-type "treasury-transfer")
            (execute-treasury-transfer (unwrap-panic (get target-amount pending)) (unwrap-panic (get target-recipient pending)))
            (if (is-eq action-type "update-parameter")
              (execute-parameter-update (unwrap-panic (get parameter-update pending)) (unwrap-panic (get new-value pending)))
              (if (is-eq action-type "fund-rewards")
                (execute-reward-funding (unwrap-panic (get target-amount pending)))
                (if (is-eq action-type "change-admin")
                  (execute-admin-change (unwrap-panic (get target-recipient pending)))
                  false
                )
              )
            )
          )
        )
      )
      ;; Record execution
      (map-set proposal-executions
        { proposal-id: proposal-id }
        {
          action-type: action-type,
          parameters: {
            amount: (get target-amount pending),
            recipient: (get target-recipient pending),
            parameter-name: (get parameter-update pending),
            parameter-value: (get new-value pending)
          },
          executed-at: current-height,
          executed-by: tx-sender,
          execution-result: execution-result
        }
      )
      ;; Remove from pending
      (map-delete pending-executions { proposal-id: proposal-id })
      (if execution-result (ok true) ERR-EXECUTION-FAILED)
    )
  )
)

;; Execution handlers for different action types
(define-private (execute-treasury-transfer (amount uint) (recipient principal))
  (let
    (
      (current-balance (var-get treasury-balance))
    )
    (if (>= current-balance amount)
      (begin
        (var-set treasury-balance (- current-balance amount))
        (is-ok (as-contract (stx-transfer? amount tx-sender recipient)))
      )
      false
    )
  )
)

(define-private (execute-parameter-update (param-name (string-ascii 30)) (new-value uint))
  (if (is-eq param-name "execution-delay")
    (begin (var-set execution-delay new-value) true)
    (if (is-eq param-name "min-participation")
      (begin (var-set min-participation-threshold new-value) true)
      false
    )
  )
)

(define-private (execute-reward-funding (amount uint))
  (is-ok (contract-call? .VoteLock fund-reward-pool amount))
)

(define-private (execute-admin-change (new-admin principal))
  (begin
    (var-set execution-admin new-admin)
    true
  )
)

;; Fund treasury for future executions
(define-public (fund-treasury (amount uint))
  (let
    (
      (current-balance (var-get treasury-balance))
    )
    (asserts! (> amount u0) ERR-INVALID-PARAMETERS)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set treasury-balance (+ current-balance amount))
    (ok amount)
  )
)

;; Emergency cancel execution (admin only)
(define-public (cancel-execution (proposal-id uint))
  (begin
    (asserts! (is-eq tx-sender (var-get execution-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? pending-executions { proposal-id: proposal-id })) ERR-PROPOSAL-NOT-FOUND)
    (map-delete pending-executions { proposal-id: proposal-id })
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-executable-action (action-type (string-ascii 30)))
  (map-get? executable-actions { action-type: action-type })
)

(define-read-only (get-pending-execution (proposal-id uint))
  (map-get? pending-executions { proposal-id: proposal-id })
)

(define-read-only (get-execution-record (proposal-id uint))
  (map-get? proposal-executions { proposal-id: proposal-id })
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-execution-admin)
  (var-get execution-admin)
)

(define-read-only (get-execution-delay)
  (var-get execution-delay)
)

(define-read-only (can-execute-proposal (proposal-id uint))
  (let
    (
      (pending (map-get? pending-executions { proposal-id: proposal-id }))
      (current-height stacks-block-height)
    )
    (match pending
      execution-data (>= current-height (get execution-eligible-at execution-data))
      false
    )
  )
)

;; Initialize the system
(initialize-action-types)
