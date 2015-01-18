(define-library (flex-table-to-scm)
  (export main-program)
  (import (scheme base)
          (scheme process-context)
          (scheme write)
          (scheme file)
          (srfi 37)
          (snow bytevector)
          (seth string-read-write)
          (seth cout)
          (seth binary-pack)
          (seth port-extras)
          (seth flex)
          )

  (begin
    ;; http://flex.sourceforge.net/manual/Tables-File-Format.html

    (define (usage why)
      (parameterize
       ((current-output-port (current-error-port)))
       (cout why "\n")
       (cout "flex-table-to-scm [options] table-name\n")
       (cout "  -i --in           input-file-name\n")
       (cout "  -o --out          output-file-name\n")
       (cout "  -v --verbose\n")
       (exit 1)))


    (define table-id->name~ '((#x1 yytd-id-accept)
                              (#x2 yytd-id-base)
                              (#x3 yytd-id-chk)
                              (#x4 yytd-id-def)
                              (#x5 yytd-id-ec)
                              (#x6 yytd-id-meta)
                              (#x7 yytd-id-nul-trans)
                              (#x8 yytd-id-nxt)
                              (#x9 yytd-id-rule-can-match-eol)
                              (#xa yytd-id-start-state-list)
                              (#xb yytd-id-transition)
                              (#xc yytd-id-acclist)))
    (define (table-id->name td-id)
      (let ((p (assv td-id table-id->name~)))
        (cond ((eq? p #f) 'unknown)
              (else (cadr p)))))


    (define (read-1d-table bytes-per-value data td-lolen idx cb verbose)
      (let ((result (make-vector td-lolen))
            (unpacker (cond ((= bytes-per-value 1) unpack-u8)
                            ((= bytes-per-value 2) unpack-u16)
                            ((= bytes-per-value 4) unpack-u32)
                            (else (error "bad bytes-per-value")))))
        (let loop ((idx idx)
                   (i 0))
          (cond ((= i td-lolen)
                 (cb result)
                 (cond (verbose (cout "\n")))
                 idx)
                (else
                 (loop (unpacker data idx (lambda (v)
                                            (cond (verbose (cout v " ")))
                                            (vector-set! result i v)))
                       (+ i 1)))))))



    (define (read-table data idx cb verbose)
      (let* ((td-id #f)
             (idx (unpack-u16 data idx (lambda (v) (set! td-id v))))
             (td-flags #f)
             (idx (unpack-u16 data idx (lambda (v) (set! td-flags v))))

             ;; flex info page has these two backwards.
             (td-hilen #f)
             (idx (unpack-u32 data idx (lambda (v) (set! td-hilen v))))
             (td-lolen #f)
             (idx (unpack-u32 data idx (lambda (v) (set! td-lolen v))))
             )
        (cond (verbose
               (cout "------------\n")
               (cout "td-id=" td-id "=" (table-id->name td-id) "\n")
               (cout "td-flags=" td-flags "\n")
               (cout "td-lolen=" td-lolen "\n")
               (cout "td-hilen=" td-hilen "\n")))

        (cond ((and (<= td-id 9) (= td-flags 1) (= td-hilen 0))
               (read-1d-table 1 data td-lolen idx (lambda (table)
                                                    (cb td-id table)) verbose))
              ((and (<= td-id 9) (= td-flags 2) (= td-hilen 0))
               (read-1d-table 2 data td-lolen idx (lambda (table)
                                                    (cb td-id table)) verbose))
              ((and (<= td-id 9) (= td-flags 4) (= td-hilen 0))
               (read-1d-table 4 data td-lolen idx (lambda (table)
                                                    (cb td-id table)) verbose))
              (else
               (cout "td-id=" td-id " td-flags=" td-flags
                     " td-hilen=" td-hilen "\n" (current-error-port))
               (error "no code for this.")
               idx))))


    (define (round-idx-to-64-bit idx verbose)
      (cond (verbose
             (cout "idx=" idx " --> " (+ idx (- 8 (modulo idx 8))) "\n")))
      (cond ((= (modulo idx 8) 0) idx)
            (else (+ idx (- 8 (modulo idx 8))))))


    (define (table-name->variable table-name)
      (string->symbol
       ;; (string-replace-string table-name "_" "-")
       (string-map
        (lambda (c) (if (eqv? c #\_) #\- c))
        table-name)
       ))


    (define (output-vector tree start length outp)
      (cout "(vector" outp)
      (let loop ((i start))
        (cond ((= i (+ start length))
               (cout ")" outp))
              (else
               (cout " " outp)
               (write (vector-ref tree i) outp)
               (loop (+ i 1))))))


    (define (output-long-vector tree outp)
      (cout "(vector-append\n" outp)
      (let ((len (vector-length tree)))
        (let loop ((i 0))
          (cond ((< (- len i) 500)
                 (output-vector tree i (- len i) outp)
                 (cout ")\n" outp))
                ((>= i len)
                 (cout ")\n" outp))
                (else
                 (output-vector tree i 500 outp)
                 (loop (+ i 500)))))))


    ;; can't just use write, because bigloo's compiler doesn't like #(...),
    ;; even though bigloo's (write) produces them.  Chicken's interpreter
    ;; has a limit on parameter-list length (though the compiler doesn't).
    ;; (define (writer tree outp)
    ;;   (cond ((and (vector? tree) (< (vector-length tree) 500))
    ;;          (output-vector tree 0 (vector-length tree) outp))
    ;;         ((vector? tree) ;; a long vector
    ;;          (output-long-vector tree outp))
    ;;         ((list? tree)
    ;;          (cout "(" outp)
    ;;          (let loop ((tree tree))
    ;;            (cond ((null? tree) (cout ")" outp))
    ;;                  (else
    ;;                   (writer (car tree) outp)
    ;;                   (cond ((not (null? (cdr tree)))
    ;;                          (cout " " outp)))
    ;;                   (loop (cdr tree))))))
    ;;         ((symbol? tree)
    ;;          (cout (symbol->string tree) outp))
    ;;         ((integer? tree)
    ;;          (cout (number->string tree) outp))
    ;;         ((string? tree)
    ;;          (cout "\""
    ;;                (string-replace-string tree "\"" "\\\"")
    ;;                "\"" outp))
    ;;         ((boolean? tree)
    ;;          (if tree (cout "#t" outp) (cout "#f" outp)))
    ;;         (else
    ;;          (cout (write-to-string tree) "\n" (current-error-port))
    ;;          (error "what?"))))



    (define options
      (list
       (option '(#\i "in") #t #f
               (lambda (option name arg
                               in-filename out-filename lexer-name verbose)
                 (if in-filename (usage "use -i only once"))
                 (values arg out-filename lexer-name verbose)))

       (option '(#\o "out") #t #f
               (lambda (option name arg
                               in-filename out-filename lexer-name verbose)
                 (if out-filename (usage "use -o only once"))
                 (values in-filename arg lexer-name verbose)))

       (option '(#\v "verbose") #f #f
               (lambda (option name arg
                               in-filename out-filename lexer-name verbose)
                 (values in-filename out-filename lexer-name #t)))

       (option '(#\h "help") #f #f
               (lambda (option name arg . seeds)
                 (usage "")))))



    (define (main-program)

      (let-values
          (((in-filename out-filename lexer-name verbose)
            (args-fold ;; srfi-37
             (cdr (command-line))
             options
             ;; unrecognized
             (lambda (option name arg . seeds)
               (usage (string-append "Unrecognized option:"
                                     (if (string? name) name (string name))
                                     "\n\n")))
             ;; operand (arguments that don't start with a hyphen)
             (lambda (operand in-filename out-filename lexer-name verbose)
               (if lexer-name (usage "only give table-name once"))
               (values in-filename out-filename operand verbose))
             #f ;; initial value of in-filename
             #f ;; initial value of out-filename
             #f ;; initial value of lexer-name
             #f ;; initial value of verbose
             )))

        (if (not in-filename) (usage "-i is required") #t)
        (if (not out-filename) (usage "-o is required") #t)
        (if (not lexer-name) (usage "table-name is required") #t)

        (let* ((inp (open-binary-input-file in-filename))
               (data (read-all-u8 inp))
               (flex-tables (make-empty-flex-tables))
               (magic (bytevector-copy data 0 4))
               (hsize #f)
               (ssize #f)
               (flags #f))

          (unpack-u32 data 4 (lambda (v) (set! hsize v)))
          (unpack-u32 data 8 (lambda (v) (set! ssize v)))
          (unpack-u16 data 12 (lambda (v) (set! flags v)))

          (cond ((not (equal? (bytevector #xf1 #x3c #x57 #xb1) magic))
                 (cout "magic number didn't match: "
                       (bytes->hex-string magic) "\n" (current-error-port))
                 (exit 1)))

          (cond (verbose
                 (cout "magic=" (bytes->hex-string magic) "\n")
                 (cout "hsize=" hsize "\n")
                 (cout "ssize=" ssize "\n")))

          (let* ((idx 14)
                 (flex-version #f)
                 (idx (unpack-str0 data idx
                                   (lambda (v) (set! flex-version v))))
                 (table-name #f)
                 (idx (unpack-str0 data idx
                                   (lambda (v) (set! table-name v)))))

            (cond (verbose
                   (cout "flex-version=" flex-version "\n")
                   (cout "table-name=" table-name "\n")))

            (flex-tables-set-name! flex-tables table-name)
            (flex-tables-set-flags! flex-tables flags)
            (flex-tables-set-flex-version! flex-tables flex-version)

            (let loop ((idx (round-idx-to-64-bit idx verbose)))
              (cond
               ((>= idx (bytevector-length data)) #t)
               (else
                (let ((idx
                       (read-table
                        data idx
                        (lambda (table-id table)
                          (let ((table-name (table-id->name table-id)))
                            (cond
                             ((eq? table-name 'yytd-id-accept)
                              (flex-tables-set-accept! flex-tables table))
                             ((eq? table-name 'yytd-id-base)
                              (flex-tables-set-base! flex-tables table))
                             ((eq? table-name 'yytd-id-chk)
                              (flex-tables-set-chk! flex-tables table))
                             ((eq? table-name 'yytd-id-def)
                              (flex-tables-set-def! flex-tables table))
                             ((eq? table-name 'yytd-id-ec)
                              (flex-tables-set-ec! flex-tables table))
                             ((eq? table-name 'yytd-id-meta)
                              (flex-tables-set-meta! flex-tables table))
                             ((eq? table-name 'yytd-id-nul-trans)
                              (flex-tables-set-nul-trans! flex-tables table))
                             ((eq? table-name 'yytd-id-nxt)
                              (flex-tables-set-nxt! flex-tables table))
                             ((eq? table-name 'yytd-id-rule-can-match-eol)
                              (flex-tables-set-rule-can-match-eol!
                               flex-tables table))
                             ((eq? table-name 'yytd-id-start-state-list)
                              (flex-tables-set-start-state-list!
                               flex-tables table))
                             ((eq? table-name 'yytd-id-transition)
                              (flex-tables-set-transition! flex-tables table))
                             ((eq? table-name 'yytd-id-acclist)
                              (flex-tables-set-acclist! flex-tables table))
                             (else
                              (error "bad table-id" table-id)))))
                        verbose)))
                  (loop (round-idx-to-64-bit idx verbose))))))

            (let* ((outp (open-output-file out-filename)))

              (cout ";; AUTOGENERATED -- DO NOT EDIT\n\n" outp)

              (write
               `(define ,(string->symbol lexer-name)
                  (make-flex-tables
                   ,(flex-tables-name flex-tables)
                   ,(flex-tables-flags flex-tables)
                   ,(flex-tables-flex-version flex-tables)
                   ,(flex-tables-accept flex-tables)
                   ,(flex-tables-base flex-tables)
                   ,(flex-tables-chk flex-tables)
                   ,(flex-tables-def flex-tables)
                   ,(flex-tables-ec flex-tables)
                   ,(flex-tables-meta flex-tables)
                   ,(flex-tables-nul-trans flex-tables)
                   ,(flex-tables-nxt flex-tables)
                   ,(flex-tables-rule-can-match-eol flex-tables)
                   ,(flex-tables-start-state-list flex-tables)
                   ,(flex-tables-transition flex-tables)
                   ,(flex-tables-acclist flex-tables)))
               outp)
              (newline outp)
              (close-output-port outp))))))
    ))
