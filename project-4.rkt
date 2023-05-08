#lang racket
(require racket/port)
(require data/maybe)

; Open the file as a single long string
(define (file-to-string f)
  (define o (open-input-file f #:mode 'text))
  (define p (port->string o))
  (close-input-port o)
  p)

; Remove any special characters and replace them with spaces
(define (normalize-string string [chars '("." "?" "!" "," ";" ":" "\"" "-" "*" "[" "]" "#" "@" "<" ">" "\t" "\n" ")" "(")] [rep " "])
  (if (empty? chars)
      string
      (normalize-string (string-replace string (first chars) rep) (rest chars))))

; Uppercase the string and split it on spaces into words
(define (get-normalized-text f)
  (string-split (string-upcase (normalize-string (file-to-string f)))))

; Takes a sorted list of strings and turns them into a hash table with the word as a key and freqency as value
(define (list-to-hash in-lst [out-tbl '()])
  (if (empty? in-lst)
      out-tbl
      (if (empty? out-tbl)
          (list-to-hash (rest in-lst) (append out-tbl (list (cons (first in-lst) 1))))
          (if (string=? (first in-lst) (car (last out-tbl)))
              (list-to-hash (rest in-lst) (append (reverse (cdr (reverse out-tbl))) (list (cons (first in-lst) (+ (cdr (last out-tbl)) 1)))))
              (list-to-hash (rest in-lst) (append out-tbl (list (cons (first in-lst) 1))))))))

; Turn the values in the hash tables from freqency into their inverse logs
(define (create-table f)
  (hash-count-to-inv-log (list-to-hash (sort (get-normalized-text f) string<?)) (length (get-normalized-text f))))

; Actual log function
(define (count-to-inv-log count total)
  (* (log (/ count total) 10) -1))

; Recursively apply this to each value in the table
(define (hash-count-to-inv-log hash total [out '()])
  (if (empty? hash)
      out
      (hash-count-to-inv-log (rest hash) total (append out (list (cons (car (first hash)) (count-to-inv-log (cdr (first hash)) total)))))))

; Compare function for the filter
(define (cmp-key f-hash s-hash)
  (string=? (car (first f-hash)) (car s-hash)))

; compare the tables
(define (compare-tables f-tbl s-tbl [sim 0] [count 0])
  (if (empty? f-tbl)
      (/ sim count)
      (if (empty? (filter (λ (ent) (cmp-key f-tbl ent)) s-tbl))
          (compare-tables (rest f-tbl) s-tbl sim count)
          (compare-tables (rest f-tbl) s-tbl (+ sim (abs (- (cdr (first f-tbl)) (cdr (first (filter (λ (ent) (cmp-key f-tbl ent)) s-tbl)))))) (+ count 1)))))

(define (rec-filter table w-list)
  (if (empty? w-list)
      table
      (rec-filter (filter (λ (w) (not (string=? (first w-list) (car w)))) table) (rest w-list))))

(define (filter-table filename filter-list)
  (cons (path->string filename) (rec-filter (create-table filename) filter-list)))

(define (open-files list-files filter-list [out '()])
  (if (empty? list-files)
      out
      (open-files (rest list-files) filter-list (append out (filter-table (first list-files) filter-list)))))

(define (get-files dir filter-list)
  (define o (directory-list dir #:build? #t))
  (define p (open-files o filter-list))
  ;(close-input-port o)
  p)

(define (get-stop-words dir filter-list-path)
  (get-files dir (get-normalized-text filter-list-path)))

(define (train dir filter-file-path [out-port-path '"table.b"])
  (define p (get-stop-words dir filter-file-path))
  (define out (open-output-file out-port-path #:exists 'replace))
  (write p out)
  (close-output-port out))

(define (read-table [in-port-path '"table.b"])
  (read (open-input-file in-port-path)))

;(train "Files" "stop_words_english.txt")

(define (text-search query text)
  (if (empty? text)
      0
      (if (string=? query (car (first text)))
          (cdr text)
          (text-search query (rest text)))))

(define (word-search query field [out '()])
  (if (empty? field)
      out
      (word-search query (rest field) (append out (word-search query (rest field))))))

(define (append-path field result [out '()])
  (if (empty? field)
      out
      (append-path (rest field) (rest result) (append out (cons (first field) (first result))))))

(define (search query)
  (let ([table (read-table)])
    (append-path table (word-search (string-upcase query) table))))

(search "accuse")