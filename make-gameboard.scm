(load "make-row.scm")

;Creates the gameboard
(define (make-gameboard%)
  (new  
   (class object%
     (field (vertical-row (make-vector 24))) ;Creates a vector with position that represents rows
     (super-new)
     
     ;Fills the vector with 24 rows containing columns
     (define/public (set-up-vectors)
       (define (loop-vectors a)
         (cond 
           ((< a 24) (send this put-in-vertical-row (make-row% (+ a 1)) a)
                     (loop-vectors (+ a 1)))
           (else (send (send this get-row 24) fill-row))))
       (loop-vectors 0))
     
     ;Sets up initial gameboard
     (set-up-vectors)
     
     ;Puts a row in a the vector for rows
     (define/public (put-in-vertical-row row place)
       (vector-set! vertical-row place row))
     
     ;Gets a specific row in gameboard
     (define/public (get-row nr)
       (vector-ref vertical-row (- nr 1)))
     
     ;Gets a coordinate in the matrix that is the logic gameboard
     (define/public (get-position row col)
       (send (send this get-row row) get-stone col))
     
     ;Checks to see if a new position is free from stones on rotate
     ;returns true if just one of them returns true 
     (define/public (check-position shape-vector row col)
       ;returns #t if the space is free
       (or (eq? (send (send this get-row row) get-stone col) 0) 
           ;returns #t if theres a stone from the current shape in the new position (needed for rotate in shape)
           (eq? (send (send this get-row row) get-stone col) (vector-ref shape-vector 0))
           (eq? (send (send this get-row row) get-stone col) (vector-ref shape-vector 1))
           (eq? (send (send this get-row row) get-stone col) (vector-ref shape-vector 2))
           (eq? (send (send this get-row row) get-stone col) (vector-ref shape-vector 3))
           ))
     
     ;Help function to find-full-row, checks if its any full rows, and 
     (define (find-row-helper row)
       (cond
         ((= row 0) (send *player* new-shape) 
                    (send *player* start-timer));after the check, a new shape is starting to drop on the board
         ((send (send *gameboard* get-row row) full-row?) ; findes a full row
          (send (send *gameboard* get-row row) request-new-stones) ;tells the row to delete itself and sends the rows above one row down
          (send *player* add-100-score!))
         (else (find-row-helper (- row 1)))
         ))
     
     ;Sees if a row is full and initiates a drop-down of all the stones
     (define/public (find-full-row)
           (find-row-helper 23))
     
     ;Sees if there are any stones in the top 3 rows, these 3 rows are not visible for the player, 
     ;if there are any stones in the top 3 rows the timer will stop, and the game will end.
     (define/public (check-top-3)
       (send *player* stop-timer)
       (if (or (send (send this get-row 1) any-stone?)
               (send (send this get-row 2) any-stone?)
               (send (send this get-row 3) any-stone?))
           (send *player* game-over)
           (send this find-full-row)))
     
     )
   ))

;defines gameboard that is used in the game
(define *gameboard* (make-gameboard%))


