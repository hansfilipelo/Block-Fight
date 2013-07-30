(load "make-stone.scm")

(define (make-T-shape% color)
  (new 
   (class object%
     (init init-rotation)
     (init init-color)
     (field (stones (make-vector 4))) ;Makes a vector for stones
     (super-new)
          
     ;Defines the current rotation of a stone
     (define current-rotation init-rotation)
     ;Defines the new-color for the shape
     (define new-color init-color)
     ;Defines a new variable
     (define current-color null)
     ;Findes the color corresponding to the init-color number
     (send this find-new-color)
      
     ;defines first position of the shape, this is current-rotation 0.
     (vector-set! stones 0 (make-stone% 2 6 current-color))
     (vector-set! stones 1 (make-stone% 2 7 current-color))
     (vector-set! stones 2 (make-stone% 2 8 current-color))
     (vector-set! stones 3 (make-stone% 3 7 current-color))
     
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
       (cond ;checks witch rotation-state it's in
         ((and (eq? current-rotation 0)
               (send stone1 check-down)
               (send stone3 check-down)
               (send stone4 check-down))
          (drop-help))
         ((and (eq? current-rotation 1)
               (send stone4 check-down)
               (send stone3 check-down))
          (drop-help))
         ((and (eq? current-rotation 2)
              (send stone1 check-down)
              (send stone2 check-down)
              (send stone3 check-down))
          (drop-help))
         ((and (eq? current-rotation 3)
               (send stone1 check-down)
               (send stone4 check-down))
          (drop-help))
         (else 
          (send *player* add-10-score!) 
          (send *gameboard* check-top-3))))
     
     ;Makes the shape fall down untill a stone gets in the way 
     (define/public (fall)
       (cond ;checks witch rotation-state it's in
         ((and (eq? current-rotation 0)
               (send stone1 check-down)
               (send stone3 check-down)
               (send stone4 check-down))
          (drop-help)
          (send this fall)
          (send *player* add-10-score!));this gives the player more points for falling from longer distance 
         ((and (eq? current-rotation 1)
               (send stone4 check-down)
               (send stone3 check-down))
          (drop-help)
          (send this fall)
          (send *player* add-10-score!));this gives the player more points for falling from longer distance 
         ((and (eq? current-rotation 2)
               (send stone1 check-down)
               (send stone2 check-down)
               (send stone3 check-down))
          (drop-help)
          (send this fall)
          (send *player* add-10-score!));this gives the player more points for falling from longer distance 
         ((and (eq? current-rotation 3)
               (send stone1 check-down)
               (send stone4 check-down))
          (drop-help)
          (send this fall)
          (send *player* add-10-score!));this gives the player more points for falling from longer distance 
         (else 
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
       (cond ;checks witch rotation-state it's in
         ((and (= current-rotation 0)
               (send stone1 check-left)
               (send stone4 check-left))
          (left-helper))
         ((and (= current-rotation 1)
               (send stone1 check-left)
               (send stone3 check-left)
               (send stone4 check-left))
          (left-helper))
         ((and (= current-rotation 2)
               (send stone4 check-left)
               (send stone3 check-left))
          (left-helper))
         ((and (= current-rotation 3)
               (send stone1 check-left)
               (send stone2 check-left)
               (send stone3 check-left))
          (left-helper))))
     
     ;Help-function to move right, adding the position of each stone with one column
     (define (right-helper)
       (send stone1 move! (send stone1 get-current-row) (+ (send stone1 get-current-col) 1))
       (send stone2 move! (send stone2 get-current-row) (+ (send stone2 get-current-col) 1))
       (send stone3 move! (send stone3 get-current-row) (+ (send stone3 get-current-col) 1))
       (send stone4 move! (send stone4 get-current-row) (+ (send stone4 get-current-col) 1)))
     
         ;Move right
     (define/public (move-right)
       (cond ;checks witch rotation-state it's in
         ((and (= current-rotation 0)
               (send stone3 check-right)
               (send stone4 check-right))
          (right-helper))
         ((and (= current-rotation 1)
               (send stone1 check-right)
               (send stone2 check-right)
               (send stone3 check-right))
          (right-helper))
         ((and (= current-rotation 2)
               (send stone1 check-right)
               (send stone4 check-right))
          (right-helper))
         ((and (= current-rotation 3)
               (send stone1 check-right)
               (send stone3 check-right)
               (send stone4 check-right))
          (right-helper))))
;-----------------------------------ROTATION OF THE SHAPE-------------------------------------------
     ;Helper for rotate
     ;checks witch rotation-state it's in, and if the new positions are free from other stones,
     ;in that case move the stones of the shape. Notice that stone nr 2 is fixed.
     ;Helps rotate
     (define (rotate-helper)
       (cond
         ;If the shape is an T
         ((and (eq? current-rotation 0)
               (send *gameboard* check-position stones (- (send stone1 get-current-row) 1) (+ (send stone1 get-current-col) 1))
               (send *gameboard* check-position stones (+ (send stone3 get-current-row) 1) (- (send stone3 get-current-col) 1))
               (send *gameboard* check-position stones(- (send stone4 get-current-row) 1) (- (send stone4 get-current-col) 1)))
          (send stone1 move! (- (send stone1 get-current-row) 1) (+ (send stone1 get-current-col) 1))
          (send stone3 move! (+ (send stone3 get-current-row) 1) (- (send stone3 get-current-col) 1))
          (send stone4 move! (- (send stone4 get-current-row) 1) (- (send stone4 get-current-col) 1))
          (set! current-rotation 1))
          ;If the shape has rotated 90 degrees
          ((and (eq? current-rotation 1)
                (send *gameboard* check-position stones (+ (send stone1 get-current-row) 1) (+ (send stone1 get-current-col) 1))
                (send *gameboard* check-position stones (- (send stone3 get-current-row) 1) (- (send stone3 get-current-col) 1))
                (send *gameboard* check-position stones (- (send stone4 get-current-row) 1) (+ (send stone4 get-current-col) 1)))
           (send stone1 move! (+ (send stone1 get-current-row) 1) (+ (send stone1 get-current-col) 1))
           (send stone3 move! (- (send stone3 get-current-row) 1) (- (send stone3 get-current-col) 1))
           (send stone4 move! (- (send stone4 get-current-row) 1) (+ (send stone4 get-current-col) 1))
           (set! current-rotation 2))
          ;If the shape has rotated 180 degrees
          ((and (eq? current-rotation 2)
                (send *gameboard* check-position stones (+ (send stone1 get-current-row) 1) (- (send stone1 get-current-col) 1))
                (send *gameboard* check-position stones (- (send stone3 get-current-row) 1) (+ (send stone3 get-current-col) 1))
                (send *gameboard* check-position stones (+ (send stone4 get-current-row) 1) (+ (send stone4 get-current-col) 1)))
           (send stone1 move! (+ (send stone1 get-current-row) 1) (- (send stone1 get-current-col) 1))
           (send stone3 move! (- (send stone3 get-current-row) 1) (+ (send stone3 get-current-col) 1))
           (send stone4 move! (+ (send stone4 get-current-row) 1) (+ (send stone4 get-current-col) 1))
           (set! current-rotation 3))
          ;If the shape has rotated 270 degrees
          ((and (send *gameboard* check-position stones (- (send stone1 get-current-row) 1) (- (send stone1 get-current-col) 1))
                (send *gameboard* check-position stones (+ (send stone3 get-current-row) 1) (+ (send stone3 get-current-col) 1))
                (send *gameboard* check-position stones (+ (send stone4 get-current-row) 1) (- (send stone4 get-current-col) 1)))
                (send stone1 move! (- (send stone1 get-current-row) 1) (- (send stone1 get-current-col) 1))
                (send stone3 move! (+ (send stone3 get-current-row) 1) (+ (send stone3 get-current-col) 1))
                (send stone4 move! (+ (send stone4 get-current-row) 1) (- (send stone4 get-current-col) 1))
                (set! current-rotation 0))))
     
     ;Rotates the shape 90 degrees clockwise
     (define/public (rotate)
       (cond
         ((and (eq? current-rotation 1)
               (eq? (send stone2 get-current-col) 11))
          (send this move-left)
          (rotate-helper))
         ((and (eq? current-rotation 3)
               (eq? (send stone2 get-current-col) 2))
          (send this move-right)
          (rotate-helper))
         (else (rotate-helper))
         ))
     ) 
   (init-rotation 0)
   (init-color color)))
       
       