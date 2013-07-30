(load "make-stone.scm")

(define (make-O-shape% color)
  (new 
   (class object%
     (init init-color)
     (field (stones (make-vector 4)))
     (super-new)

     ;Defines the new-color for the shape
     (define new-color init-color)
     ;Defines a new variable
     (define current-color null)
     ;Findes the color corresponding to the init-color number
     (send this find-new-color)
     
     ;Defines stones in the shape and puts them on the gameboard 
       (vector-set! stones 0 (make-stone% 1 6 current-color))
       (vector-set! stones 1 (make-stone% 1 7 current-color))
       (vector-set! stones 2 (make-stone% 2 6 current-color))
       (vector-set! stones 3 (make-stone% 2 7 current-color))
     
     ;Gives the stones in the shape aliases for easy access from inside shape
     (define stone1 (vector-ref stones 0))
     (define stone2 (vector-ref stones 1))
     (define stone3 (vector-ref stones 2))
     (define stone4 (vector-ref stones 3))
     
     ;sets the color of the shape
     (define/public (find-new-color)
       (cond
         ((= new-color 0) (set! current-color "green"))
         ((= new-color 1) (set! current-color "red"))
         ((= new-color 2) (set! current-color "yellow"))
         ((= new-color 3) (set! current-color "blue"))
         ((= new-color 4) (set! current-color "purple"))
         ((= new-color 5) (set! current-color "orange"))
         ))
     

;-------------------------------MOVES THE SHAPE VERTICALLY-----------------------------------
     ;help-function to drop-down
     (define (drop-help)
       (send stone4 drop-down)
       (send stone3 drop-down) 
       (send stone2 drop-down) 
       (send stone1 drop-down))
    
     ;Moves all stones in this shape one step down
     (define/public (drop-down)
       (if (and (send stone3 check-down)
                (send stone4 check-down))
           (drop-help)
           (begin
             (send *gameboard* check-top-3)
             (send *player* add-10-score!))))
     
     ;Makes the shape fall down untill a stone gets in the way
     (define/public (fall)
       (if (and (send stone3 check-down)
                (send stone4 check-down))
           (begin
             (drop-help)
             (send this fall)
             (send *player* add-10-score!));this gives the player more points for falling from longer distance
           (begin 
             (send *gameboard* check-top-3)
             (send *player* add-10-score!))))
     
;-----------------------------------MOVES THE SHAPE HORISONTALLY-------------------------------------------   
     ;Help to move-left, substracting the position of each stone with one column
     (define (left-helper)
       (send stone1 move! (send stone1 get-current-row) (- (send stone1 get-current-col) 1))
       (send stone2 move! (send stone2 get-current-row) (- (send stone2 get-current-col) 1))
       (send stone3 move! (send stone3 get-current-row) (- (send stone3 get-current-col) 1))
       (send stone4 move! (send stone4 get-current-row) (- (send stone4 get-current-col) 1)))
     
     ;Move left
     (define/public (move-left)
       (if (and (send stone1 check-left)
                (send stone3 check-left))
           (left-helper)))
     
     ;Help-function to move right, adding the position of each stone with one column
     (define (right-helper)
       (send stone1 move! (send stone1 get-current-row) (+ (send stone1 get-current-col) 1))
       (send stone2 move! (send stone2 get-current-row) (+ (send stone2 get-current-col) 1))
       (send stone3 move! (send stone3 get-current-row) (+ (send stone3 get-current-col) 1))
       (send stone4 move! (send stone4 get-current-row) (+ (send stone4 get-current-col) 1)))
     
     ;Move right
     (define/public (move-right)
       (if (and (send stone2 check-right)
                (send stone4 check-right))
           (right-helper)))
;-----------------------------------------ROTATION--------------------------------------------------
     ;Rotate is just defined, its only purpose is to prevent error when trying to rotate the shape while playing
     (define/public (rotate)
       (if (= 1 2)
           ())) 
     ) 
   (init-color color)))     
