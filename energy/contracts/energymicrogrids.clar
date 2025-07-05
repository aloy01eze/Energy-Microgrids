;; Decentralized Energy Microgrids
;; A system allowing rural communities to trade locally-produced solar energy

;; Error codes
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ALREADY_REGISTERED (err u101))
(define-constant ERR_NOT_REGISTERED (err u102))
(define-constant ERR_INSUFFICIENT_ENERGY (err u103))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_INVALID_PRINCIPAL (err u106))

;; Data maps
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

(define-map transactions
  { tx-id: uint }
  {
    producer: principal,
    consumer: principal,
    energy-amount: uint,
    price: uint,
    timestamp: uint
  }
)

;; Variables
(define-data-var platform-fee-percent uint u5) ;; 0.5%
(define-data-var platform-wallet principal tx-sender)
(define-data-var transaction-counter uint u0)

;; Read-only functions
(define-read-only (get-producer (address principal))
  (map-get? producers { address: address })
)

(define-read-only (get-consumer (address principal))
  (map-get? consumers { address: address })
)

(define-read-only (get-transaction (tx-id uint))
  (map-get? transactions { tx-id: tx-id })
)

(define-read-only (is-producer (address principal))
  (is-some (map-get? producers { address: address }))
)

(define-read-only (is-consumer (address principal))
  (is-some (map-get? consumers { address: address }))
)

(define-read-only (get-platform-fee-percent)
  (var-get platform-fee-percent)
)

(define-read-only (get-platform-wallet)
  (var-get platform-wallet)
)

;; Public functions
(define-public (register-producer (price-per-unit uint))
  (begin
    (asserts! (not (is-producer tx-sender)) ERR_ALREADY_REGISTERED)
    (asserts! (not (is-consumer tx-sender)) ERR_ALREADY_REGISTERED)
    (asserts! (> price-per-unit u0) ERR_INVALID_AMOUNT)
    
    (map-set producers
      { address: tx-sender }
      {
        energy-available: u0,
        price-per-unit: price-per-unit,
        total-energy-sold: u0,
        total-earnings: u0
      }
    )
    (ok true)
  )
)

(define-public (register-consumer)
  (begin
    (asserts! (not (is-consumer tx-sender)) ERR_ALREADY_REGISTERED)
    (asserts! (not (is-producer tx-sender)) ERR_ALREADY_REGISTERED)
    
    (map-set consumers
      { address: tx-sender }
      {
        total-energy-bought: u0,
        total-spent: u0
      }
    )
    (ok true)
  )
)

(define-public (add-energy (amount uint))
  (let (
    (producer-data (unwrap! (get-producer tx-sender) ERR_NOT_REGISTERED))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (map-set producers
      { address: tx-sender }
      (merge producer-data {
        energy-available: (+ (get energy-available producer-data) amount)
      })
    )
    (ok true)
  )
)

(define-public (update-price (new-price uint))
  (let (
    (producer-data (unwrap! (get-producer tx-sender) ERR_NOT_REGISTERED))
  )
    (asserts! (> new-price u0) ERR_INVALID_AMOUNT)
    
    (map-set producers
      { address: tx-sender }
      (merge producer-data {
        price-per-unit: new-price
      })
    )
    (ok true)
  )
)

(define-public (purchase-energy (producer-address principal) (energy-amount uint) (max-price uint))
  (let (
    (consumer-data (unwrap! (get-consumer tx-sender) ERR_NOT_REGISTERED))
    (producer-data (unwrap! (get-producer producer-address) ERR_NOT_REGISTERED))
    (price-per-unit (get price-per-unit producer-data))
    (total-price (* energy-amount price-per-unit))
    (platform-fee (/ (* total-price (var-get platform-fee-percent)) u1000))
    (producer-payment (- total-price platform-fee))
    (tx-id (+ (var-get transaction-counter) u1))
  )
    ;; Validate the transaction
    (asserts! (>= (get energy-available producer-data) energy-amount) ERR_INSUFFICIENT_ENERGY)
    (asserts! (<= total-price max-price) ERR_INSUFFICIENT_PAYMENT)
    
    ;; Transfer STX from consumer to producer and platform
    (try! (stx-transfer? total-price tx-sender (var-get platform-wallet)))
    (try! (stx-transfer? producer-payment (var-get platform-wallet) producer-address))
    
    ;; Update producer data
    (map-set producers
      { address: producer-address }
      (merge producer-data {
        energy-available: (- (get energy-available producer-data) energy-amount),
        total-energy-sold: (+ (get total-energy-sold producer-data) energy-amount),
        total-earnings: (+ (get total-earnings producer-data) producer-payment)
      })
    )
    
    ;; Update consumer data
    (map-set consumers
      { address: tx-sender }
      (merge consumer-data {
        total-energy-bought: (+ (get total-energy-bought consumer-data) energy-amount),
        total-spent: (+ (get total-spent consumer-data) total-price)
      })
    )
    
    ;; Record the transaction
    (map-set transactions
      { tx-id: tx-id }
      {
        producer: producer-address,
        consumer: tx-sender,
        energy-amount: energy-amount,
        price: total-price,
        timestamp: stacks-block-height
      }
    )
    
    ;; Update transaction counter
    (var-set transaction-counter tx-id)
    
    (ok tx-id)
  )
)

;; Admin functions
(define-public (update-platform-fee (new-fee-percent uint))
  (begin
    (asserts! (is-eq tx-sender (var-get platform-wallet)) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee-percent u50) ERR_INVALID_AMOUNT) ;; Max 5%
    
    (var-set platform-fee-percent new-fee-percent)
    (ok true)
  )
)

;; Fixed function with proper validation for new-wallet
(define-public (update-platform-wallet (new-wallet principal))
  (begin
    (asserts! (is-eq tx-sender (var-get platform-wallet)) ERR_NOT_AUTHORIZED)
    ;; Add validation for the new wallet
    (asserts! (is-ok (principal-destruct? new-wallet)) ERR_INVALID_PRINCIPAL)
    ;; Ensure new wallet is not the zero address or other invalid values
    (asserts! (not (is-eq new-wallet 'SP000000000000000000002Q6VF78)) ERR_INVALID_PRINCIPAL)
    
    (var-set platform-wallet new-wallet)
    (ok true)
  )
)