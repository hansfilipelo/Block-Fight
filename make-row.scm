(require (lib "racket/vector.ss"))

(define (make-row% row)
  (new 
   (class object%
     (init row-nr)
     ;Creates vector with positions that represents columns
     (field (columns (make-vector 12))) 
     (super-new)
     
     ;This is the rows number counting from the top
     (define current-row row-nr)
     
     ;Creates boards on the gameboard
     (vector-set! columns 0 1)
     (vector-set! columns 11 1)

;Stones------------------------------------------------------
    ;Gets info
     ;Sees if there is a stone on a column
     (define/public (stone-in-col? col)
       (if (eq? (vector-ref columns (- col 1)) 0)
           #f
           #t))
     
     ;Gets a stone from a given column in the row
     (define/public (get-stone col)
       (vector-ref columns (- col 1)))
     
          ;Gets all columns from the current-row 
     (define/public (all-columns)
       columns)
     
     ;Sets info--------
     
     ;Adds a stone to a given column in the row
     (define/public (add-stone! col stone)
           (vector-set! columns (- col 1) stone))
     
          ;Removes a stone from a specific position
     (define/public (remove-stone! stone col)
       (if (eq? stone (send this get-stone col))
           (vector-set! columns (- col 1) 0)))
     
     ;Fills row with only the number 1
     (define/public (fill-row)
       (define (fill-help col)
         (cond
           ((= col 0) (vector-set! columns col 1))
           (else (vector-set! columns col 1) (fill-help (- col 1)))))
       (fill-help 11))
     
     ;Checks if this row is full
     (define/public (full-row?)
       (if (= (vector-count (lambda (n) (eq? n 0)) columns) 0)
           #t
           #f)) 
     
     ;Requests new stones from the row above
     (define/public (request-new-stones)
       (cond 
         ((= current-row 1)
          (set! columns (make-vector 12))
          (vector-set! columns 0 1)
          (vector-set! columns 11 1) 
          (send *gameboard* find-full-row))
         (else (send (send *gameboard* get-row (- current-row 1)) send-stones-down))))
     
     ;Takes a new vector from the row above
     (define/public (set-new-stones! vector)
       (set! columns vector))
     
     ;Sends stones to the row below
     (define/public (send-stones-down)
       (cond
         ((= current-row 1) (send (send *gameboard* get-row (+ current-row 1)) set-new-stones! columns)
                            (set! columns (make-vector 12))
                            (vector-set! columns 0 1)
                            (vector-set! columns 11 1)
                            (send *gameboard* find-full-row))
         (else (send (send *gameboard* get-row (+ current-row 1)) set-new-stones! columns)
               (send (send *gameboard* get-row (- current-row 1)) send-stones-down))
         ))
     
     ;Sees if there is ANY stone in the row
     (define/public (any-stone?)
       (if (< (vector-count (lambda (n) (eq? n 0)) columns) 10)
           #t
           #f))
;Row-nr------------------------------------------------------
     ;Gets the current rows number
     (define/public (get-row-nr)
       current-row)
     
     )
   (row-nr row)))