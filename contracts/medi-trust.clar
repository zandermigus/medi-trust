;; ---------------------------------------------------------
;; MediTrust - On-Chain Healthcare Subsidy Protocol
;; ---------------------------------------------------------
;; Functionality: Healthcare Subsidy Protocol
;; Author: Nathaniel Matthew
;; Description: Transparent on-chain healthcare subsidy pools 
;; that allow donors to contribute, patients to apply, 
;; validators to approve, and direct payouts in STX.
;; ---------------------------------------------------------

;; Fixed trait implementation syntax and added proper contract owner
;; (impl-trait .nft-trait.nft-trait) ;; For donor proof NFTs (SIP-009) - commented out for now

;; Contract owner
;; Set contract-owner to the deployer principal (replace with actual deployer principal before deployment)
(define-constant contract-owner 'ST000000000000000000002AMW42H)

;; Error codes
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u401))
(define-constant err-insufficient-balance (err u402))
(define-constant err-not-approved (err u403))
(define-constant err-not-found (err u404))
(define-constant err-already-approved (err u405))
(define-constant err-cap-exceeded (err u406))
(define-constant err-pool-inactive (err u407))

;; Helper to get contract principal (for as-contract usage)
(define-read-only (get-contract-owner)
  contract-owner
)

;; -------------------------
;; Data Structures
;; -------------------------

;; Healthcare pool structure
(define-map subsidy-pools
  {id: uint}
  {
    creator: principal,
    balance: uint,
    cap: uint,
    active: bool
  }
)

;; Patient applications
(define-map applications
  {id: uint}
  {
    applicant: principal,
    pool-id: uint,
    claim-hash: (buff 64), ;; e.g., IPFS/Arweave hash
    amount: uint,
    approved: bool,
    disbursed: bool
  }
)

;; Global counters
(define-data-var pool-counter uint u0)
(define-data-var app-counter uint u0)

;; Validators (NGOs, hospitals)
(define-map validators
  {address: principal}
  {
    active: bool
  }
)

;; -------------------------
;; Functions
;; -------------------------

;; Add a new validator (only contract owner for now)
(define-public (add-validator (validator principal))
  (if (is-eq tx-sender contract-owner)
      (begin
        (map-insert validators {address: validator} {active: true})
        (print {event: "validator-added", validator: validator})
        (ok true)
      )
      err-owner-only
  )
)

;; Create a subsidy pool
(define-public (create-pool (cap uint))
  (let ((id (+ (var-get pool-counter) u1)))
    (begin
      (var-set pool-counter id)
      (map-insert subsidy-pools {id: id}
        { creator: tx-sender, balance: u0, cap: cap, active: true })
      (print {event: "pool-created", pool-id: id, creator: tx-sender, cap: cap})
      (ok id)
    )
  )
)

;; Fixed donate function - transfer STX from donor to contract
(define-public (donate (pool-id uint) (amount uint))
  (let ((pool (map-get? subsidy-pools {id: pool-id})))
    (let ((pool-data (unwrap! pool err-not-found)))
      (if (get active pool-data)
          (if (<= (+ (get balance pool-data) amount) (get cap pool-data))
              (begin
                ;; Transfer STX from donor to contract
                (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
                (map-set subsidy-pools {id: pool-id}
                  (merge pool-data { balance: (+ (get balance pool-data) amount) }))
                (print {event: "donated", pool-id: pool-id, donor: tx-sender, amount: amount})
                (ok true)
              )
              err-cap-exceeded
          )
          err-pool-inactive
      )
    )
  )
)

;; Patient applies for subsidy
(define-public (apply (pool-id uint) (claim-hash (buff 64)) (amount uint))
  (let ((id (+ (var-get app-counter) u1)))
    (begin
      (var-set app-counter id)
      (map-insert applications {id: id}
        { applicant: tx-sender, pool-id: pool-id, claim-hash: claim-hash,
          amount: amount, approved: false, disbursed: false })
      (print {event: "applied", app-id: id, applicant: tx-sender, pool-id: pool-id})
      (ok id)
    )
  )
)

;; Validator approves application
(define-public (approve (app-id uint))
  (let ((val (map-get? validators {address: tx-sender})))
    (let ((validator-data (unwrap! val err-unauthorized)))
      (if (get active validator-data)
          (let ((app (map-get? applications {id: app-id})))
            (let ((app-data (unwrap! app err-not-found)))
              (if (not (get approved app-data))
                  (begin
                    (map-set applications {id: app-id}
                      (merge app-data { approved: true }))
                    (print {event: "approved", app-id: app-id, validator: tx-sender})
                    (ok true)
                  )
                  err-already-approved
              )
            )
          )
          err-unauthorized
      )
    )
  )
)

;; Fixed disburse function - added missing closing parenthesis and corrected STX transfer
(define-public (disburse (app-id uint))
  (let ((app (map-get? applications {id: app-id})))
    (let ((app-data (unwrap! app err-not-found)))
      (if (and (get approved app-data) (not (get disbursed app-data)))
          (let ((pool (map-get? subsidy-pools {id: (get pool-id app-data)})))
            (let ((pool-data (unwrap! pool err-not-found)))
              (if (>= (get balance pool-data) (get amount app-data))
                  (begin
                    ;; Transfer STX from contract to applicant
                    (try! (as-contract (stx-transfer? (get amount app-data) tx-sender (get applicant app-data))))
                    (map-set subsidy-pools {id: (get pool-id app-data)}
                      (merge pool-data { balance: (- (get balance pool-data) (get amount app-data)) }))
                    (map-set applications {id: app-id}
                      (merge app-data { disbursed: true }))
                    (print {event: "disbursed", app-id: app-id, recipient: (get applicant app-data), amount: (get amount app-data)})
                    (ok true)
                  )
                  err-insufficient-balance
              )
            )
          )
          err-not-approved
      )
    )
  )
)

;; Read-only functions for querying data
(define-read-only (get-pool (pool-id uint))
  (map-get? subsidy-pools {id: pool-id})
)

(define-read-only (get-application (app-id uint))
  (map-get? applications {id: app-id})
)

(define-read-only (is-validator (address principal))
  (let ((val (map-get? validators {address: address})))
    (if (is-some val)
        (get active (unwrap! val false))
        false
    )
  )
)

(define-read-only (get-pool-count)
  (var-get pool-counter)
)

(define-read-only (get-app-count)
  (var-get app-counter)
)

;; -------------------------
;; Future extensions
;; - NFT proof-of-donation
;; - DAO-based validator approval
;; - Auto-disbursement scheduling
;; -------------------------
