;; NEW FEATURE: Energy Subscription Service
;; This feature allows consumers to subscribe to regular energy deliveries from producers
;; Provides predictable revenue streams and automated recurring transactions

;; Import existing error codes from base contract
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ALREADY_REGISTERED (err u101))
(define-constant ERR_NOT_REGISTERED (err u102))
(define-constant ERR_INSUFFICIENT_ENERGY (err u103))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_INVALID_PRINCIPAL (err u106))

;; New Error codes for Subscription Service
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u300))
(define-constant ERR_SUBSCRIPTION_ALREADY_EXISTS (err u301))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u302))
(define-constant ERR_SUBSCRIPTION_PAUSED (err u303))
(define-constant ERR_INSUFFICIENT_SUBSCRIPTION_BALANCE (err u304))
(define-constant ERR_INVALID_SUBSCRIPTION_DURATION (err u305))

;; Base contract data maps (these would reference your existing contract)
(define-map producers  
  { address: principal }  
  {    
    energy-available: uint,    
    price-per-unit: uint,    
    total-energy-sold: uint,    
    total-earnings: uint  
  }
)

(define-map consumers  
  { address: principal }  
  {    
    total-energy-bought: uint,    
    total-spent: uint  
  }
)

;; Base contract variables
(define-data-var platform-wallet principal tx-sender)

;; Helper functions to check registration (these would call your existing contract)
(define-read-only (get-producer (address principal))
  (map-get? producers { address: address })
)

(define-read-only (get-consumer (address principal))
  (map-get? consumers { address: address })
)

;; Data Maps for Energy Subscription Service
(define-map energy-subscriptions
  { subscription-id: uint }
  {
    consumer: principal,
    producer: principal,
    energy-amount-per-period: uint,
    price-per-unit: uint,
    payment-frequency: uint, ;; in blocks (daily = 144 blocks)
    total-periods: uint,
    periods-completed: uint,
    next-payment-block: uint,
    subscription-balance: uint,
    status: (string-ascii 10), ;; "active", "paused", "expired", "cancelled"
    created-at: uint,
    last-payment: uint
  }
)

(define-map subscription-payments
  { payment-id: uint }
  {
    subscription-id: uint,
    consumer: principal,
    producer: principal,
    energy-amount: uint,
    payment-amount: uint,
    payment-block: uint,
    period-number: uint
  }
)

;; Variables for Subscription Service
(define-data-var subscription-counter uint u0)
(define-data-var payment-counter uint u0)
(define-data-var subscription-fee-percent uint u2) ;; 0.2% fee for subscription service

;; Create Energy Subscription
(define-public (create-energy-subscription 
    (producer-address principal) 
    (energy-amount-per-period uint) 
    (payment-frequency uint) 
    (total-periods uint)
    (initial-payment uint))
  (let (
    (subscription-id (+ (var-get subscription-counter) u1))
    (producer-data (unwrap! (get-producer producer-address) ERR_NOT_REGISTERED))
    (consumer-data (unwrap! (get-consumer tx-sender) ERR_NOT_REGISTERED))
    (price-per-unit (get price-per-unit producer-data))
    (total-cost (* (* energy-amount-per-period price-per-unit) total-periods))
  )
    ;; Validate inputs
    (asserts! (> energy-amount-per-period u0) ERR_INVALID_AMOUNT)
    (asserts! (>= payment-frequency u144) ERR_INVALID_SUBSCRIPTION_DURATION) ;; Minimum 1 day
    (asserts! (> total-periods u0) ERR_INVALID_SUBSCRIPTION_DURATION)
    (asserts! (>= initial-payment total-cost) ERR_INSUFFICIENT_PAYMENT)
    
    ;; Transfer initial payment from consumer
    (try! (stx-transfer? initial-payment tx-sender (as-contract tx-sender)))
    
    ;; Create subscription using the generated ID (not user input)
    (map-set energy-subscriptions
      { subscription-id: subscription-id }
      {
        consumer: tx-sender,
        producer: producer-address,
        energy-amount-per-period: energy-amount-per-period,
        price-per-unit: price-per-unit,
        payment-frequency: payment-frequency,
        total-periods: total-periods,
        periods-completed: u0,
        next-payment-block: (+ stacks-block-height payment-frequency),
        subscription-balance: initial-payment,
        status: "active",
        created-at: stacks-block-height,
        last-payment: u0
      }
    )
    
    ;; Update subscription counter
    (var-set subscription-counter subscription-id)
    
    (ok subscription-id)
  )
)

;; Process Subscription Payment (can be called by anyone to trigger payment)
(define-public (process-subscription-payment (subscription-id uint))
  (begin
    ;; Validate subscription ID bounds
    (asserts! (> subscription-id u0) ERR_SUBSCRIPTION_NOT_FOUND)
    (asserts! (<= subscription-id (var-get subscription-counter)) ERR_SUBSCRIPTION_NOT_FOUND)
    
    (let (
      (subscription (unwrap! (map-get? energy-subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND))
      (payment-id (+ (var-get payment-counter) u1))
      (energy-amount (get energy-amount-per-period subscription))
      (price-per-unit (get price-per-unit subscription))
      (payment-amount (* energy-amount price-per-unit))
      (subscription-fee (/ (* payment-amount (var-get subscription-fee-percent)) u1000))
      (producer-payment (- payment-amount subscription-fee))
      (producer-address (get producer subscription))
      (consumer-address (get consumer subscription))
    )
      ;; Validate subscription can be processed
      (asserts! (is-eq (get status subscription) "active") ERR_SUBSCRIPTION_PAUSED)
      (asserts! (>= stacks-block-height (get next-payment-block subscription)) ERR_SUBSCRIPTION_EXPIRED)
      (asserts! (>= (get subscription-balance subscription) payment-amount) ERR_INSUFFICIENT_SUBSCRIPTION_BALANCE)
      (asserts! (< (get periods-completed subscription) (get total-periods subscription)) ERR_SUBSCRIPTION_EXPIRED)
      
      ;; Check if producer has enough energy
      (let ((producer-data (unwrap! (get-producer producer-address) ERR_NOT_REGISTERED)))
        (asserts! (>= (get energy-available producer-data) energy-amount) ERR_INSUFFICIENT_ENERGY)
        
        ;; Update producer energy and earnings
        (map-set producers
          { address: producer-address }
          (merge producer-data {
            energy-available: (- (get energy-available producer-data) energy-amount),
            total-energy-sold: (+ (get total-energy-sold producer-data) energy-amount),
            total-earnings: (+ (get total-earnings producer-data) producer-payment)
          })
        )
      )
      
      ;; Update consumer data
      (let ((consumer-data (unwrap! (get-consumer consumer-address) ERR_NOT_REGISTERED)))
        (map-set consumers
          { address: consumer-address }
          (merge consumer-data {
            total-energy-bought: (+ (get total-energy-bought consumer-data) energy-amount),
            total-spent: (+ (get total-spent consumer-data) payment-amount)
          })
        )
      )
      
      ;; Transfer payment to producer
      (try! (as-contract (stx-transfer? producer-payment tx-sender producer-address)))
      
      ;; Transfer fee to platform
      (try! (as-contract (stx-transfer? subscription-fee tx-sender (var-get platform-wallet))))
      
      ;; Record payment
      (map-set subscription-payments
        { payment-id: payment-id }
        {
          subscription-id: subscription-id,
          consumer: consumer-address,
          producer: producer-address,
          energy-amount: energy-amount,
          payment-amount: payment-amount,
          payment-block: stacks-block-height,
          period-number: (+ (get periods-completed subscription) u1)
        }
      )
      
      ;; Update subscription
      (let (
        (new-periods-completed (+ (get periods-completed subscription) u1))
        (new-balance (- (get subscription-balance subscription) payment-amount))
        (new-status (if (>= new-periods-completed (get total-periods subscription)) "expired" "active"))
      )
        (map-set energy-subscriptions
          { subscription-id: subscription-id }
          (merge subscription {
            periods-completed: new-periods-completed,
            next-payment-block: (+ stacks-block-height (get payment-frequency subscription)),
            subscription-balance: new-balance,
            status: new-status,
            last-payment: stacks-block-height
          })
        )
      )
      
      ;; Update payment counter
      (var-set payment-counter payment-id)
      
      (ok payment-id)
    )
  )
)

;; Pause Subscription (only consumer can pause)
(define-public (pause-subscription (subscription-id uint))
  (begin
    ;; Validate subscription ID bounds
    (asserts! (> subscription-id u0) ERR_SUBSCRIPTION_NOT_FOUND)
    (asserts! (<= subscription-id (var-get subscription-counter)) ERR_SUBSCRIPTION_NOT_FOUND)
    
    (let (
      (subscription (unwrap! (map-get? energy-subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
      (asserts! (is-eq tx-sender (get consumer subscription)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status subscription) "active") ERR_SUBSCRIPTION_PAUSED)
      
      (map-set energy-subscriptions
        { subscription-id: subscription-id }
        (merge subscription { status: "paused" })
      )
      
      (ok true)
    )
  )
)

;; Resume Subscription (only consumer can resume)
(define-public (resume-subscription (subscription-id uint))
  (begin
    ;; Validate subscription ID bounds
    (asserts! (> subscription-id u0) ERR_SUBSCRIPTION_NOT_FOUND)
    (asserts! (<= subscription-id (var-get subscription-counter)) ERR_SUBSCRIPTION_NOT_FOUND)
    
    (let (
      (subscription (unwrap! (map-get? energy-subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
      (asserts! (is-eq tx-sender (get consumer subscription)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status subscription) "paused") ERR_SUBSCRIPTION_PAUSED)
      
      (map-set energy-subscriptions
        { subscription-id: subscription-id }
        (merge subscription { 
          status: "active",
          next-payment-block: (+ stacks-block-height (get payment-frequency subscription))
        })
      )
      
      (ok true)
    )
  )
)

;; Cancel Subscription and Refund Remaining Balance (only consumer can cancel)
(define-public (cancel-subscription (subscription-id uint))
  (begin
    ;; Validate subscription ID bounds
    (asserts! (> subscription-id u0) ERR_SUBSCRIPTION_NOT_FOUND)
    (asserts! (<= subscription-id (var-get subscription-counter)) ERR_SUBSCRIPTION_NOT_FOUND)
    
    (let (
      (subscription (unwrap! (map-get? energy-subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND))
      (remaining-balance (get subscription-balance subscription))
      (consumer-address (get consumer subscription))
    )
      (asserts! (is-eq tx-sender consumer-address) ERR_NOT_AUTHORIZED)
      (asserts! (not (is-eq (get status subscription) "cancelled")) ERR_SUBSCRIPTION_EXPIRED)
      
      ;; Refund remaining balance to consumer
      (if (> remaining-balance u0)
        (try! (as-contract (stx-transfer? remaining-balance tx-sender consumer-address)))
        true
      )
      
      ;; Update subscription status
      (map-set energy-subscriptions
        { subscription-id: subscription-id }
        (merge subscription { 
          status: "cancelled",
          subscription-balance: u0
        })
      )
      
      (ok remaining-balance)
    )
  )
)

;; Top up Subscription Balance
(define-public (topup-subscription (subscription-id uint) (amount uint))
  (begin
    ;; Validate subscription ID bounds
    (asserts! (> subscription-id u0) ERR_SUBSCRIPTION_NOT_FOUND)
    (asserts! (<= subscription-id (var-get subscription-counter)) ERR_SUBSCRIPTION_NOT_FOUND)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (let (
      (subscription (unwrap! (map-get? energy-subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
      (asserts! (is-eq tx-sender (get consumer subscription)) ERR_NOT_AUTHORIZED)
      
      ;; Transfer additional payment from consumer
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update subscription balance
      (map-set energy-subscriptions
        { subscription-id: subscription-id }
        (merge subscription {
          subscription-balance: (+ (get subscription-balance subscription) amount)
        })
      )
      
      (ok true)
    )
  )
)

;; Read-only functions for Subscription Service
(define-read-only (get-subscription (subscription-id uint))
  (if (and (> subscription-id u0) (<= subscription-id (var-get subscription-counter)))
    (map-get? energy-subscriptions { subscription-id: subscription-id })
    none
  )
)

(define-read-only (get-subscription-payment (payment-id uint))
  (if (and (> payment-id u0) (<= payment-id (var-get payment-counter)))
    (map-get? subscription-payments { payment-id: payment-id })
    none
  )
)

(define-read-only (get-subscription-fee-percent)
  (var-get subscription-fee-percent)
)

(define-read-only (is-subscription-due (subscription-id uint))
  (if (and (> subscription-id u0) (<= subscription-id (var-get subscription-counter)))
    (match (map-get? energy-subscriptions { subscription-id: subscription-id })
      subscription (and 
        (is-eq (get status subscription) "active")
        (>= stacks-block-height (get next-payment-block subscription))
        (< (get periods-completed subscription) (get total-periods subscription))
      )
      false
    )
    false
  )
)

(define-read-only (get-subscription-stats (subscription-id uint))
  (if (and (> subscription-id u0) (<= subscription-id (var-get subscription-counter)))
    (match (map-get? energy-subscriptions { subscription-id: subscription-id })
      subscription (ok {
        total-paid: (* (get periods-completed subscription) (* (get energy-amount-per-period subscription) (get price-per-unit subscription))),
        remaining-periods: (- (get total-periods subscription) (get periods-completed subscription)),
        next-payment-due: (get next-payment-block subscription),
        is-due: (>= stacks-block-height (get next-payment-block subscription))
      })
      ERR_SUBSCRIPTION_NOT_FOUND
    )
    ERR_SUBSCRIPTION_NOT_FOUND
  )
)

;; Get subscription counter (for external validation)
(define-read-only (get-subscription-counter)
  (var-get subscription-counter)
)

;; Get payment counter (for external validation)
(define-read-only (get-payment-counter)
  (var-get payment-counter)
)

;; Admin function to update subscription fee
(define-public (update-subscription-fee (new-fee-percent uint))
  (begin
    (asserts! (is-eq tx-sender (var-get platform-wallet)) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee-percent u10) ERR_INVALID_AMOUNT) ;; Max 1%
    
    (var-set subscription-fee-percent new-fee-percent)
    (ok true)
  )
)

;; Emergency function to pause all subscriptions (admin only)
(define-public (emergency-pause-all-subscriptions)
  (begin
    (asserts! (is-eq tx-sender (var-get platform-wallet)) ERR_NOT_AUTHORIZED)
    ;; This would iterate through all active subscriptions and pause them
    ;; Implementation would depend on your indexing strategy
    (ok true)
  )
)
