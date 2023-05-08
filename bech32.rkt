#lang racket
(require racket/list)

(module+ test
  (require rackunit
           rackunit/text-ui))

; bech32 constants
(define bech32-bitcoin-hrp "bc")
(define bech32-testnet-hrp "tb")
(define bech32-separator "1")
(define bech32chars "qpzry9x8gf2tvdw0s3jn54khce6mua7l")
(define BECH32-CONST 1)
(define BECH32M-CONST #x2bc830a3)

(define (pad-str str-val)
  (~a str-val #:min-width 5 #:pad-string "0"))

(define (pad-int-binarystr int-val pad-val)
  (~r int-val #:base 2 #:min-width pad-val #:pad-string "0"))

(define (pad-int-hexstr int-val pad-val)
  (~r int-val #:base 16 #:min-width pad-val #:pad-string "0"))

; used in bech32 format, take a str with binary data, return list of nb bytes str
(define (splitnbpart str nb)
  (define (splitnb lst acc)
    (cond
      [(= (length lst) 0) acc]
      [(< (length lst) nb) (cons lst acc)]
      [else (splitnb (list-tail lst nb) (cons (take lst nb) acc))]))
  (define list-splitnb (splitnb (string->list str) '()))
  (define list-string (map list->string list-splitnb))
  (reverse list-string))

; used in bech32 format
(define (hexbytes-to-hex5bit hexstring)
  (define char-list (string->list hexstring))
  (define str-list (map string char-list))
  (define nb-list (map (lambda (nb) (string->number nb 16)) str-list))
  (define binary-list (map (lambda (val) (pad-int-binarystr val 4)) nb-list))
  (define concac-list (string-append* binary-list))
  (define splitlist (splitnbpart concac-list 5))
  (define list-str (list-update splitlist (- (length splitlist) 1) pad-str))
  (define binarylistnew (map (lambda (nb) (string->number nb 2)) list-str))
  binarylistnew)

(define (expandhrp hrp)
  (define charlist (string->list hrp))
  (define char-val (map char->integer charlist))
  (define left-val (map (lambda (nb) (arithmetic-shift nb -5)) char-val))
  (define right-val (map (lambda (nb) (bitwise-and nb 31)) char-val))
  (append left-val '(0) right-val))

(define (bech32-polymod int-list) ; int-list is a list of int
  (define GEN '(#x3b6a57b2 #x26508e6d #x1ea119fa #x3d4233dd #x2a1462b3))
  (define chk 1)
  (define (chk-xor-gen chk-v gen-val inc b-val)
    (bitwise-xor
     chk-v
     (if (= (bitwise-and (arithmetic-shift b-val (- inc)) 1) 1) gen-val 0)))
  (define (update-chk-value chk-val value)
    (define b-val (arithmetic-shift chk-val -25))
    (define (update-chk-with-GEN gen acc inc)
      (if (equal? gen '())
          acc
          (update-chk-with-GEN (cdr gen)
                               (chk-xor-gen acc (car gen) inc b-val)
                               (+ 1 inc))))
    (update-chk-with-GEN
     GEN
     (bitwise-xor (arithmetic-shift (bitwise-and chk-val #x1ffffff) 5) value)
     0))
  (define (polymod-compute values-list acc)
    (if (equal? values-list '())
        acc
        (polymod-compute (cdr values-list)
                         (update-chk-value acc (car values-list)))))
  (polymod-compute int-list chk))

(define (generate-checksum-bech32 int-list
                                  #:version [version 1]
                                  #:hrp [hrp bech32-bitcoin-hrp])
  (define int-list-with-hrp (append (expandhrp hrp) int-list))
  (define const-val (if (= version 0) BECH32-CONST BECH32M-CONST))
  (define polymod
    (bitwise-xor (bech32-polymod (append int-list-with-hrp '(0 0 0 0 0 0)))
                 const-val))
  (define checksum '(0 1 2 3 4 5))
  (map
   (lambda (i) (bitwise-and (arithmetic-shift polymod (- (* 5 (- 5 i)))) 31))
   checksum))

(define (bech32-encode value
                       #:version [version 1]
                       #:hrp [hrp bech32-bitcoin-hrp]) ; take a hashvalue
  (define hex5bit (hexbytes-to-hex5bit value))
  (define val-list (cons version hex5bit)) ; witness version
  (define checksum
    (generate-checksum-bech32 val-list #:version version #:hrp hrp))
  (define list-and-checksum (append val-list checksum))
  (define char-list
    (map (lambda (nb) (string-ref bech32chars nb)) list-and-checksum))
  (define string-list (list->string char-list))
  (string-append hrp bech32-separator string-list))

(define (bech32-recompute-checksum hrp bech32-intlist)
  (define const (bech32-polymod (append (expandhrp hrp) bech32-intlist)))
  const)

(define (bech32-parsing bech32-str)
  ; TODO : check if the data part of the bech32 string only contains CHARSET element
  ;Decoders MUST NOT accept strings where some characters are uppercase and some are lowercase (such strings are referred to as mixed case strings).
  (when (and (not (equal? bech32-str (string-downcase bech32-str)))
             (not (equal? bech32-str (string-upcase bech32-str))))
    (error "mixed case strings are not allowed"))
  (define bech32-string (string-downcase bech32-str))
  (define hrp-compute
    (car (string-split bech32-string "1"))) ; hrp end with the "1" caracter
  ; MUST verify that the human-readable part is "bc" for mainnet and "tb" for testnet.
  ; you can adapt this for another network (for example litecoin)
  (when (and (not (equal? hrp-compute bech32-bitcoin-hrp))
             (not (equal? hrp-compute bech32-testnet-hrp)))
    (error "not a valid hrp"))
  (define separator-compute (substring bech32-string 2 3))
  (when (not (equal? separator-compute bech32-separator))
    (error "not a valid separator"))
  (define code-list (string->list bech32chars))
  (define bech32-string-vals (substring bech32-string 3))
  (define bech32-charlist (string->list bech32-string-vals))
  (define bech32-intlist
    (map (lambda (char) (index-of code-list char)) bech32-charlist))
  *
  (values hrp-compute bech32-intlist))

(define (bech32-verify-checksum hrp bech32-intlist)
  (define const (bech32-recompute-checksum hrp bech32-intlist))
  (cond
    [(or (equal? const BECH32-CONST) (equal? const BECH32M-CONST)) #t]
    [else #f]))

; value as a bech32 encoded string
(define (bech32-verify value)
  (define-values (hrp-compute bech32-intlist) (bech32-parsing value))
  (bech32-verify-checksum hrp-compute bech32-intlist))

(define (bech32-decode value)
  (define-values (hrp-compute bech32-intlist) (bech32-parsing value))
  (when (not (bech32-verify-checksum hrp-compute bech32-intlist))
    (error "checksum is not valid"))
  (define bech32const-val
    (bech32-recompute-checksum hrp-compute bech32-intlist))
  (define first-val (car bech32-intlist))
  ; MUST verify that the first decoded data value (the witness version) is between 0 and 16, inclusive.
  (when (or (> first-val 16) (< first-val 0))
    (error "first value cannot be superior to 16 or inferior to 0"))
  (when (or (and (= first-val 0) (not (= bech32const-val BECH32-CONST)))
            (and (not (= first-val 0)) (not (= bech32const-val BECH32M-CONST))))
    (error "first value do not match the checksum"))
  ; now we have our (5bit encoded) list as int
  ; convert each int in base 2
  (define bech32-intlist-no-first-val (cdr bech32-intlist))
  (define bech32-intlist-nochecksum (drop-right bech32-intlist-no-first-val 6))
  (define bech32-binarylist
    (map (lambda (nb) (pad-int-binarystr nb 5)) bech32-intlist-nochecksum))
  (define concac-list (string-append* bech32-binarylist))
  (define splitlist (splitnbpart concac-list 8))
  ; if incomplete group at the end, it must be of 4bit or less, must be all zeros, should be discarded
  (define splitlist-clean
    (cond
      [(= (string-length (last splitlist)) 8) splitlist]
      [(and (<= (string-length (last splitlist)) 4)
            (equal? (last splitlist)
                    (make-string (string-length (last splitlist)) #\0)))
       (drop-right splitlist 1)]
      [else
       (error
        "not a valid bech32 string: imcomplete group is more than 5bit")]))
  ; convert the (8 bit encoded str) into the int value
  (define intlistnew (map (lambda (nb) (string->number nb 2)) splitlist-clean))
  ;There MUST be between 2 and 40 groups, which are interpreted as the bytes of the witness program.
  (when (or (< (length intlistnew) 2) (> (length intlistnew) 40))
    (error "not a valid bech32: number of groups invalid [2:40]"))
  ; convert the int into a hex str
  ; each hexstr should be of size 2 char
  (define hexlist (map (lambda (nb) (pad-int-hexstr nb 2)) intlistnew))
  ; concat the hex
  (define concac-hex (string-append* hexlist))
  concac-hex)

(module+ test
  (define test-bech32
    (test-suite "Tests for bech32.rkt"
      (let ()
        (test-case "# Test vector 1"
          (check-equal?
           (bech32-encode "751e76e8199196d454941c45d1b3a323f1433bd6"
                          #:version 0)
           (string-downcase "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4"))
          (check-equal? (bech32-decode
                         "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4")
                        "751e76e8199196d454941c45d1b3a323f1433bd6")
          (check-true (bech32-verify
                       "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4")))
        (test-case "# Test vector 2"
          (check-equal?
           (bech32-encode
            "1863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262"
            #:version 0
            #:hrp "tb")
           "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7")
          (check-equal?
           (bech32-decode
            "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7")
           "1863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262")
          (check-true
           (bech32-verify
            "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7")))
        (test-case "# Test vector 3"
          (check-equal?
           (bech32-encode
            "751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6")
           "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kt5nd6y")
          (check-equal?
           (bech32-decode
            "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kt5nd6y")
           "751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6"))
        (test-case "# Test vector 4"
          (check-equal? (bech32-encode "751e" #:version 16)
                        (string-downcase "BC1SW50QGDZ25J"))
          (check-equal? (bech32-decode "BC1SW50QGDZ25J") "751e"))
        (test-case "# Test vector 5"
          (check-equal?
           (bech32-encode "751e76e8199196d454941c45d1b3a323" #:version 2)
           "bc1zw508d6qejxtdg4y5r3zarvaryvaxxpcs")
          (check-equal? (bech32-decode "bc1zw508d6qejxtdg4y5r3zarvaryvaxxpcs")
                        "751e76e8199196d454941c45d1b3a323"))
        (test-case "# Test vector 6"
          (check-equal?
           (bech32-encode
            "000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"
            #:version 0
            #:hrp "tb")
           "tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy")
          (check-equal?
           (bech32-decode
            "tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy")
           "000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"))
        (test-case "# Test vector 7"
          (check-equal?
           (bech32-encode
            "000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"
            #:hrp "tb")
           "tb1pqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesf3hn0c")
          (check-equal?
           (bech32-decode
            "tb1pqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesf3hn0c")
           "000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"))
        (test-case "# Test vector 8"
          (check-equal?
           (bech32-encode
            "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")
           "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0")
          (check-equal?
           (bech32-decode
            "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0")
           "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")))))
  (run-tests test-bech32))

(provide bech32-encode
         bech32-decode
         bech32-verify)
