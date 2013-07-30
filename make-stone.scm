(load "make-gameboard.scm")

(define stone%
   (class object%
     (init row)
     (init col)
     (init stone-color)
     (super-new)
     
     ;Defines variables for the stones current row and column
     (define current-row row)
     (define current-col col)
     (define current-color stone-color)
     
      ;Adds stone to gameboard
     (send (send *gameboard* get-row current-row) add-stone! current-col this)

;-----------Returns information about the stones position---------------------------------------------------------------------------------------
     
          ;Returns the stones current row
     (define/public (get-current-row)
       current-row)
     
     ;Returns the color of the stone
     (define/public (get-stone-color)
       current-color)
     
     ;Returns the stones current column
     (define/public (get-current-col)
       current-col)
     
     ;Checks to see if there is a stone under this stone
     (define/public (check-down)
       (let ((next-row (send *gameboard* get-row (+ 1 current-row))))
         (not (send next-row stone-in-col? current-col))))
     
     ;Checks to see if there is a stone to the right of this stone
     (define/public (check-right)
       (let ((current-row-ob (send *gameboard* get-row current-row)))
         (not (send current-row-ob stone-in-col? (+ 1 current-col)))))
     
     ;Checks to see if there is a stone to the left of this stone
     (define/public (check-left)
       (let ((current-row-ob (send *gameboard* get-row current-row)))
         (not (send current-row-ob stone-in-col? (- current-col 1)))))
  
;---------Moves the stone in some way--------------------------------------------------------------------------------------------------
     
     ;Moves stone to a new position
     (define/public (move! new-row new-col)
       (let ((current-row-ob (send *gameboard* get-row current-row))
             (next-row (send *gameboard* get-row new-row)))
         (cond 
          ((not (and (eq? new-row current-row) (eq? new-col current-col))) (send next-row add-stone! new-col this)
                                                                           (send current-row-ob remove-stone! this current-col)
                                                                           (set! current-row new-row)
                                                                           (set! current-col new-col))
         )))
     
          ;Moves the stone one step down on gameboard
     (define/public (drop-down)
       (let ((current-row-ob (send *gameboard* get-row current-row))
             (next-row (send *gameboard* get-row (+ 1 current-row))))
         (send next-row add-stone! current-col this)
         (send current-row-ob remove-stone! this current-col)
         (set! current-row (+ 1 current-row))))
     
     
;----------------------------------------------------------------------------------------------------------------------------------------   
     ;Ritar ut stenen
     (define/public (draw)
       (new canvas%
            (parent gameboard-canvas)
            (paint-callback
             (lambda (canvas dc)
               (send dc draw-rectangle 25 25 25 25)))))
     
          ))

(define (make-stone% row-nr col-nr color)
  (new stone% (row row-nr) (col col-nr) (stone-color color)))


;slumpar färg


      
    