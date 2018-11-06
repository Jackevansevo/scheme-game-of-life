(define (initialize-board x y)
  (make-list x (make-list y)))

(define (seed-board board)
  (map (lambda (row) 
         (map (lambda (cell) (zero? (random 2))) row)) board))

(define (cell->string cell) 
  (if cell "■ " ". "))

(define (row->string row)
  (string-append (apply string-append (map cell->string row)) "\n")) 
    
(define (board->string board)
  (apply string-append (map row->string board)))

(define (next board)
  (evolve board))

(define (identity i) i)

(define (evolve board)

  (let ((x-len (length board))
        (y-len (length (list-ref board 0))))

    (define (get-cell x y)
      ;; Returns void if cell is out of bounds
      (if (not (or (or (negative? x) (negative? y))
                   (or (>= x x-len) (>= y y-len))))
        (list-ref (list-ref board y) x)))

   (define (evolve-cell x y)
     (let* ((adjacent 
              (list (get-cell (+ x 1) y)
                    (get-cell (- x 1) y)
                    (get-cell x (+ y 1))
                    (get-cell x (- y 1))))
            (neighbors (length (filter identity adjacent))))
       (or (= neighbors 3) (and (= neighbors 2) (get-cell x y)))))

   (define (evolve-row y)
     (map (lambda (x) (evolve-cell x y)) (iota y-len)))

   (map evolve-row (iota x-len))))
