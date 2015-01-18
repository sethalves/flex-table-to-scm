(define-library (seth flex)
  (export make-flex-tables
          flex-tables-is-type?
          flex-tables-name flex-tables-set-name!
          flex-tables-flags flex-tables-set-flags!
          flex-tables-flex-version flex-tables-set-flex-version!
          flex-tables-accept flex-tables-set-accept!
          flex-tables-base flex-tables-set-base!
          flex-tables-chk flex-tables-set-chk!
          flex-tables-def flex-tables-set-def!
          flex-tables-ec flex-tables-set-ec!
          flex-tables-meta flex-tables-set-meta!
          flex-tables-nul-trans flex-tables-set-nul-trans!
          flex-tables-nxt flex-tables-set-nxt!
          flex-tables-rule-can-match-eol flex-tables-set-rule-can-match-eol!
          flex-tables-start-state-list flex-tables-set-start-state-list!
          flex-tables-transition flex-tables-set-transition!
          flex-tables-acclist flex-tables-set-acclist!
          make-empty-flex-tables
          make-tokenizer
          tokinize)
  (import (scheme base))
  (begin

    (define-record-type flex-tables
      (make-flex-tables name flags flex-version accept base chk def ec meta
                        nul-trans nxt rule-can-match-eol start-state-list
                        transition acclist)
      flex-tables-is-type?
      (name flex-tables-name flex-tables-set-name!)
      (flags flex-tables-flags flex-tables-set-flags!)
      (flex-version flex-tables-flex-version flex-tables-set-flex-version!)
      (accept flex-tables-accept flex-tables-set-accept!)
      (base flex-tables-base flex-tables-set-base!)
      (chk flex-tables-chk flex-tables-set-chk!)
      (def flex-tables-def flex-tables-set-def!)
      (ec flex-tables-ec flex-tables-set-ec!)
      (meta flex-tables-meta flex-tables-set-meta!)
      (nul-trans flex-tables-nul-trans flex-tables-set-nul-trans!)
      (nxt flex-tables-nxt flex-tables-set-nxt!)
      (rule-can-match-eol flex-tables-rule-can-match-eol
                          flex-tables-set-rule-can-match-eol!)
      (start-state-list flex-tables-start-state-list
                        flex-tables-set-start-state-list!)
      (transition flex-tables-transition flex-tables-set-transition!)
      (acclist flex-tables-acclist flex-tables-set-acclist!))

    (define (make-empty-flex-tables)
      (make-flex-tables #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))


    (define (make-tokenizer tables cb)
      (define (yy-accept idx) (vector-ref (flex-tables-accept tables) idx))
      (define (yy-base idx) (vector-ref (flex-tables-base tables) idx))
      (define (yy-ec idx) (vector-ref (flex-tables-ec tables) idx))
      (define (yy-chk idx) (vector-ref (flex-tables-chk tables) idx))
      (define (yy-def idx) (vector-ref (flex-tables-def tables) idx))
      (define (yy-meta idx) (vector-ref (flex-tables-meta tables) idx))
      (define (yy-nxt idx) (vector-ref (flex-tables-nxt tables) idx))

      ;; https://idlebox.net/2008/apidocs/flex-2.5.35.zip/flex_26.html
      ;; There is a scheme in the Aho/Sethi/Ullman compiler book for
      ;; compressing scanner tables.  It involves creating two pairs of tables.
      ;; The first has "base" and "default" entries, the second has "next" and
      ;; "check" entries.  The "base" entry is indexed by the current state and
      ;; yields an index into the next/check table.  The "default" entry gives
      ;; what to do if the state transition isn't found in next/check.  The
      ;; "next" entry gives the next state to enter, but only if the "check"
      ;; entry verifies that this entry is correct for the current state.  Flex
      ;; creates templates of series of next/check entries and then encodes
      ;; differences from these templates as a way to compress the tables.

      (define (vector-max v)
        (let loop ((i 0) (r 0))
          (if (= i (vector-length v)) r
              (let ((v (vector-ref v i)))
                (loop (+ i 1) (if (> v r) v r))))))

      (define do-meta-state
        ;; (+ (apply max (vector->list (flex-tables-nxt tables))) 1))
        (+ (vector-max (flex-tables-nxt tables)) 1))

      (define stop-state
        ;; (apply max (vector->list (flex-tables-base tables)))
        (vector-max (flex-tables-base tables)))

      (define data (make-bytevector 0))

      (define (deliver-token action start-pos end-pos)
        (cb action (bytevector-copy data start-pos end-pos)))

      (define (scan yy-current-state yy-cp)
        (if (>= yy-cp (bytevector-length data)) yy-current-state
            (let chk-not-current-state-loop
                ((yy-current-state yy-current-state)
                 (yy-char-class (yy-ec (bytevector-u8-ref data yy-cp))))
              (let* ((yy-b (yy-base yy-current-state))
                     (chk (yy-chk (+ yy-b yy-char-class))))
                (cond ((= chk yy-current-state)
                       (let* ((base (yy-base yy-current-state)))
                         (yy-nxt (+ base yy-char-class))))
                      (else
                       (chk-not-current-state-loop
                        (yy-def yy-current-state)
                        (if (>= (yy-def yy-current-state) do-meta-state)
                            (yy-meta yy-char-class)
                            yy-char-class))))))))

      (define (drop-unneeded-data x)
        (set! data (bytevector-copy data x (bytevector-length data))))

      (define save-yy-cp 0)
      (define save-yy-last-accepting-state #f)
      (define save-yy-last-accepting-cpos #f)
      (define save-yy-bp 0)
      (define save-yy-current-state #f)

      (lambda (new-data)

        ;; (cout "stop-state=" stop-state "\n")
        ;; (cout "do-meta-state=" do-meta-state "\n")

        (cond ((eof-object? new-data) #t)
              ((bytevector? new-data)
               (set! data (bytevector-append data new-data)))
              ((string? new-data)
               (set! data (bytevector-append data (string->utf8 new-data))))
              (else
               (error "make-tokenizer -- bad new-data" new-data)))
        (let loop ((yy-cp save-yy-cp) ;; index into data
                   (yy-last-accepting-state save-yy-last-accepting-state)
                   (yy-last-accepting-cpos save-yy-last-accepting-cpos)
                   (yy-bp save-yy-bp)
                   (yy-current-state
                    (if save-yy-current-state
                        save-yy-current-state
                        (scan 1 0))))

          ;; (cout "yy-cp=" yy-cp
          ;;       " data[yy-cp]="
          ;;       (if (>= yy-cp (bytevector-length data))
          ;;           #f
          ;;           (integer->char (bytevector-u8-ref data yy-cp)))
          ;;       ;; " last-acc-state=" yy-last-accepting-state
          ;;       ;; " last-acc-cpos=" yy-last-accepting-cpos
          ;;       ;; " yy-bp=" yy-bp
          ;;       " yy-current-state=" yy-current-state
          ;;       " yy-ec=" (if (< yy-cp (bytevector-length data))
          ;;                     (yy-ec (bytevector-u8-ref data yy-cp))
          ;;                     #f)
          ;;       "\n" (current-error-port))

          (cond
           ((eof-object? new-data)
            (cond ((and yy-last-accepting-state yy-last-accepting-cpos)
                   (deliver-token (yy-accept yy-last-accepting-state)
                                  yy-bp yy-last-accepting-cpos)))
            #t)
           ((>= yy-cp (bytevector-length data))
            (set! save-yy-cp yy-cp)
            (set! save-yy-last-accepting-state yy-last-accepting-state)
            (set! save-yy-last-accepting-cpos yy-last-accepting-cpos)
            (set! save-yy-bp yy-bp)
            (set! save-yy-current-state yy-current-state)
            #t)
           ((not (= (yy-base yy-current-state) stop-state))
            (let ((acc>0 (> (yy-accept yy-current-state) 0)))
              (loop (+ yy-cp 1)
                    (if acc>0 yy-current-state yy-last-accepting-state)
                    (if acc>0 (+ yy-cp 1) yy-last-accepting-cpos)
                    yy-bp
                    (scan yy-current-state (+ yy-cp 1)))))
           (else
            (let* ((yy-act (yy-accept yy-current-state))
                   (yy-cp (+ yy-cp 1)))
              (cond ((= yy-act 0)
                     ;; back up
                     (deliver-token (yy-accept yy-last-accepting-state)
                                    yy-bp yy-last-accepting-cpos)
                     ;; (drop-unneeded-data yy-last-accepting-cpos)
                     ;; (loop 0 #f #f 0 (scan 1 0))
                     (loop yy-last-accepting-cpos #f #f
                           yy-last-accepting-cpos
                           (scan 1 yy-last-accepting-cpos))
                     )
                    (else
                     (deliver-token yy-act yy-bp yy-cp)
                     ;; (drop-unneeded-data yy-cp)
                     ;; (loop 0 #f #f 0 (scan 1 0))
                     (loop yy-cp #f #f yy-cp (scan 1 yy-cp))
                     ))))))))


    (define (tokinize tables data)
      (define result (list))
      (define (receive-token token-type token-bytes)
        ;; (cout "--> '" (latin-1->string token-bytes) "'\n")
        (set! result
              (cons (list token-type (utf8->string token-bytes)) result)))
      (let ((tokenizer (make-tokenizer tables receive-token)))
        (tokenizer data)
        (tokenizer (eof-object))
        (reverse result)))

    ))
