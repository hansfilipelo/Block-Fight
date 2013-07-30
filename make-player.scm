(load "make-gameboard.scm")
(load "make-L-shape.scm")
(load "make-T-shape.scm")
(load "make-S-shape.scm")
(load "make-O-shape.scm")
(load "make-I-shape.scm")
(load "make-Z-shape.scm")
(load "make-J-shape.scm")


;----------------------------------------PLAYER--------------------------------------------------------------
;Creates a player. 
(define (make-player%)
  (new
   (class object%
     (field (name null))
     (super-new)
     
     ;Defines current difficulty
     (define current-level 1)
     (define score 0)
     
     ;This is the highscore
     (define highscore null)
     ;Loads highscore from the file highscore.txt
     (send this load-highscore-list)
    
      
     (define current-shape null) ;This is the current shape
     (define next-shape null) ;Chooses the next shape
     (define next-shape-color null) ;Sets the color of next-shape
     
     (define mode null)
     
;---------------------Level & score---------------------------------------------------------
     ;Gets the current level
     (define/public (get-level)
       current-level)
     
     ;Sets the current level
     (define/public (level-up!)
       (set! current-level (+ 1 current-level))
       (send current-level-message set-label (number->string current-level))
       (if (= current-level 13)
       (set! background  (make-object bitmap% "Bakgrund/Background-flames.jpg")))) 
     
     ;Adds 100 points to the players current score (is done when shape sets down)
     (define/public (add-10-score!)
       (set! score (+ score (* 10 current-level)))
       (send current-score-message set-label (number->string score)))
     
     ;Adds 1000 points to the players current score (is donw when shape sets down)
          (define/public (add-100-score!)
            (set! score (+ score (* 100 current-level)))
            (send current-score-message set-label (number->string score)))
     
     ;Gets the current score
     (define/public (get-score)
       score)
     
     ;Timer that sets the current level. Starts at 1 and adds 1 every 30 seconds. 
     (define level-timer (new timer%
                              (notify-callback (lambda () 
                                                 (send this level-up!)))
                              (interval 30000)
                              (just-once? #f)))
     ;Holds the timer on game-init
     (send level-timer stop)
     
     
;-----------------------------------------------------------------------------------
     
     ;Gets the player name
     (define/public (get-player-name)
       name)
     
     ;Creates a new shapes and randomizes next shape
     (define/public (new-shape)
       (cond
         ((eq? next-shape 0) 
          (set! current-shape (make-L-shape% next-shape-color))
          (set! next-shape (random 7))
          (set! next-shape-color (random 6))
          (send next-shape-canvas on-paint))
         ((eq? next-shape 1) 
          (set! current-shape (make-I-shape% next-shape-color))
          (set! next-shape (random 7))
          (set! next-shape-color (random 6))
          (send next-shape-canvas on-paint))
         ((eq? next-shape 2) 
          (set! current-shape (make-O-shape% next-shape-color))
          (set! next-shape (random 7))
          (set! next-shape-color (random 6))
          (send next-shape-canvas on-paint))
         ((eq? next-shape 3) 
          (set! current-shape (make-T-shape% next-shape-color))
          (set! next-shape (random 7))
          (set! next-shape-color (random 6))
          (send next-shape-canvas on-paint))
         ((eq? next-shape 4) 
          (set! current-shape (make-S-shape% next-shape-color))
          (set! next-shape (random 7))
          (set! next-shape-color (random 6))
          (send next-shape-canvas on-paint))
         ((eq? next-shape 5) 
          (set! current-shape (make-Z-shape% next-shape-color))
          (set! next-shape (random 7))
          (set! next-shape-color (random 6))
          (send next-shape-canvas on-paint))
         ((eq? next-shape 6)
          (set! current-shape (make-J-shape% next-shape-color))
          (set! next-shape (random 7))
          (set! next-shape-color (random 6))
          (send next-shape-canvas on-paint))
         ))
     
     ;Gets the current shape
     (define/public (get-current-shape)
       current-shape)
     
     ;Gets the next shape
     (define/public (get-next-shape)
       next-shape)
     
     ;Gets the next-shapes color
     (define/public (get-next-shape-color)
       next-shape-color)
     
     ;Moves current shape left
     (define/public (right)
       (send current-shape move-right)
       (send gameboard-canvas on-paint))
     
     ;Moves current shape left
     (define/public (left)
       (send current-shape move-left)
       (send gameboard-canvas on-paint))
     
     ;Drops current shape one step down
     (define/public (drop-shape-down)
       (send current-shape drop-down)
       (send gameboard-canvas on-paint))
     
     ;Drops the current shape to "the ground"
     (define/public (fall-down-all-way)
       (send current-shape fall)
       (send gameboard-canvas on-paint))
     
     ;Rotates current shape
     (define/public (rotate-shape)
       (send current-shape rotate)
       (send gameboard-canvas on-paint))
     
     ;Timer that sets the speed of the game
     (define drop-timer (new timer%
                              (notify-callback (lambda () 
                                                 (send this drop-shape-down)))
                              (interval (- 425 (* 25 (send this get-level))))
                              (just-once? #f)))
     (send drop-timer stop) ;Holds timer on game-init
     
     ;Starts timer
     (define/public (start-timer)
       (send drop-timer start (- 425 (* 25 (send this get-level)))))
     
     ;Stop timer
     (define/public (stop-timer)
       (send drop-timer stop))
     
          ;Starts the game by running necessary commands
     (define/public (start-game! p-n)
       (set! background (make-object bitmap% "Bakgrund/Background-tree.jpg"))
       (set! name p-n) ;Sets player name
       (set! current-level 1) ;Resets level
       (set! score 0) ;Resets score
       (set! mode 'play) ;Sets the mode to play (other modes are pause and game-over)
       (set! next-shape (random 7)) ;Sets a next shape
       (set! next-shape-color (random 6)) ;Sets a color for the next shape
       (send *gameboard* set-up-vectors) ;Creates the logical gameboard
       (send current-name-message set-label name) ;Shows name in infopanel on the left
       (send current-score-message set-label (number->string 0)) ;Shows the score in the panel to the left
       (send current-level-message set-label (number->string 1)) ;Shows the current level in panel to the left
       (send this new-shape) ;Creates shape, game-on!
       (send next-shape-canvas on-paint) ;Draws the (presumably empty) board
       (send this start-timer) ;Starts drop-timer
       (send level-timer start 30000) ;Starts level timer
       (send gameboard-canvas focus))
     
     ;Sets highscore on game-over
     (define/public (set-highscore-list!)
       (define (loop lst rest-lst)
         (cond
           ((null? lst) (set! highscore (append highscore (list (cons name score)))))
           ((< (cdar lst) score)
            (set! highscore (append rest-lst (cons (cons name score) lst))))
           (else (loop (cdr lst) (append rest-lst (list (car lst)))))))
       (loop highscore '()))
     
     ;Saves the highscore to highscore.txt
     (define/public (save-highscore-list)
       (let ((file (open-output-file "highscore.txt" 'truncate)))
         (write highscore file)
         (close-output-port file)))
     
     ;Loads highscore from the file highscore.txt
     (define/public (load-highscore-list)
       (let ((file (open-input-file "highscore.txt")))
         (set! highscore (read file))
         (close-input-port file)))
     
     ;Other objects can reach the highscore. The Highscore dialog needs this. 
     (define/public (get-highscore)
       highscore)
     
     ;Ends the game by running necesary commands
     (define/public (game-over)
       (send this set-highscore-list!)
       (send this save-highscore-list)
       (set! mode 'game-over)
       (send level-timer stop)
       (send drop-timer stop)
       (print-loop (send *player* get-highscore))
       (send game-over-dialog show #t))
     
     ;Pauses/Resumes the game (is done using pause/resume button in GUI)
     (define/public (pause)
       (cond
         ((eq? mode 'pause) 
          (send level-timer start 30000)
          (send drop-timer start (- 425 (* 25 (send this get-level))))
          (set! mode 'play)
          (send pause-button set-label "Pause"))
         ((eq? mode 'play) 
          (send level-timer stop)
          (send drop-timer stop)
          (set! mode 'pause)
          (send pause-button set-label "Resume"))))

     ;Gets pause/play-mode
     (define/public (get-mode)
       mode)
     )))

;-----------------------------------------------------GRAPHICS-----------------------------------------------------

;-----------------------------------------Gameboard-frame------------------------------------------

;Defines the windows size
(define window-width 600)
(define window-height 600)

;; Make a frame by instantiating the frame% class
(define frame (instantiate frame% 
                ("niker917 - hanel742" 
                 #f
                 window-width 
                 window-height)))
 
;; Show the frame by calling its show method
(send frame show #t)
       

;Creates a main panel
(define main-panel (new horizontal-panel% 
                     (parent frame)
                     ))

;Creates panel to the left
(define left-panel (new vertical-panel% 
                     (parent main-panel)
                     (stretchable-width #t)))

;Highscore dialog, also shown on game-over and earlier only on game over 
;(hence all names begin with game-over-----------------------------------------------------------------------------

(define game-over-dialog (new dialog% 
                              (label "Highscore")
                              (parent frame)
                              (width 150)
                              (height 400)))
;Panel for structure
(define game-over-panel
  (new vertical-panel% 
       [parent game-over-dialog]
       [alignment '(center center)]))

;Panel for headline
  (define headline-panel 
    (new horizontal-panel% 
         [parent game-over-panel]
         [alignment '(center center)]))

  ;Prints headline
(define game-over-message
  (new message%
       (parent headline-panel)
       (label "This is the highscore - let's hope you did well")
       [vert-margin 20]	 
       [horiz-margin 75]
       (min-width 300)
       (min-height 30)
       ))

;Creates panel for score AND names
  (define highscore-panel 
    (new horizontal-panel% 
         [parent game-over-panel]
         [alignment '(center center)]))
  
  ;Creates a panel for names in highscore
  (define name-panel
    (new vertical-panel% 
         [parent highscore-panel]
         ))
  
   ;Creates a panel for scores in highscore
(define gameover-score-panel
    (new vertical-panel% 
         [parent highscore-panel]
         )) 
  
  ;Creates a message used to display names in highscore
  (define (make-name-message%) 
    (new message%
         (parent name-panel)
         (label "Highscore-name")
         (min-width 100)
         (min-height 20)
         ))
  
  ;Creates message used to display scores in highscore
  (define (make-score-message%)
      (new message%
           (parent gameover-score-panel)
           (label "Highscore")
           (min-width 100)
           (min-height 20)
         ))
    
      ;Prints the top 10 scores in the highscore
(define (score-loop lst counter)
  (cond
    ((null? lst))
    ((= counter 10) (send (make-score-message%) set-label (number->string (cdar lst))))
    (else
     (send (make-score-message%) set-label (number->string (cdar lst)))
     (score-loop (cdr lst) (+ counter 1)))
    ))
    
  ;Prints the top 10 names in the highscore
(define (name-loop lst counter)
  (cond
    ((null? lst))
    ((= counter 10) (send (make-name-message%) set-label (caar lst)))
    (else
     (send (make-name-message%) set-label (caar lst))
     (name-loop (cdr lst) (+ counter 1)))
    ))

;Starts both loops
(define (print-loop lst)
  (name-loop lst 1)
  (score-loop lst 1))
  
;OK-button for game-over-panel
 (new button% 
      [parent game-over-panel] 
      [label "OK"]
      (callback 
       (lambda (button event)
        (send game-over-dialog show #f)
         ;Deletes names from game-over-dialog
         (define (delete-names lst)
           (cond
             ((null? lst))
             (else (send name-panel delete-child (car lst))
                   (delete-names (cdr lst)))))
         ;Deltes score from game-over-dialog
         (define (delete-score lst)
           (cond
             ((null? lst))
             (else (send gameover-score-panel delete-child (car lst))
                   (delete-score (cdr lst)))))
         (delete-names (send name-panel get-children))
         (delete-score (send gameover-score-panel get-children))
         (send *player* pause))
         ))

;---------------------Dialog frame for new-game--------------------------------
; Creates new-game-dialog
(define new-game-dialog (instantiate dialog% ("New game")
                          (parent frame)))
  ; Add a text field to the dialog
  (define name-field (new text-field% [parent new-game-dialog] [label "Your name"]))

; Add a horizontal panel to the dialog, with centering for buttons
  (define button-panel (new horizontal-panel% [parent new-game-dialog]
                                       [alignment '(center center)]))

; Add Cancel and Ok buttons to the horizontal panel (new button% [parent panel] [label "Cancel"])
  (new button% 
     [parent button-panel] 
     [label "Ok"]
     (callback 
      (lambda (button event)
        (send *player* start-game! (send name-field get-value))
        (send new-game-dialog show #f))))
  ;Cancel
(new button% 
     (parent button-panel)
     (label "Cancel")
     (callback (lambda (button event)
       (send new-game-dialog show #f))))
;Changes position i system says so
(when (system-position-ok-before-cancel?)
    (send button-panel change-children reverse))

;----------------------------------------MENU-BUTTONS----------------------------------------------

;Makes a new-game-button
(instantiate button% ()
  (label "New game")
  (parent left-panel)
  (min-width 125)
  (callback (lambda (button event)
              (send new-game-dialog show #t))))

;Makes Pause/resume-button
(define pause-button 
  (instantiate button% ()
    (label "Pause")
    (parent left-panel)
    (min-width 125)
    (callback (lambda (button event)
                (send *player* pause)))))

;Makes Highscore-button
(instantiate button% ()
  (label "Highscore")
  (parent left-panel)
  (min-width 125)
  (callback (lambda (button event)
              (send *player* pause)
              (send *player* load-highscore-list)
              (print-loop (send *player* get-highscore))
              (send game-over-dialog show #t))))

;; Makes a Quit-button
(instantiate button% () 
  (label "Quit") 
  (parent left-panel)
  (min-width 125)
             ;; Callback procedure for a button click
  (callback (lambda (button event)
              (send frame show #f)
              (if (eq? (send *player* get-mode) 'play) 
                  (send *player* pause)))))

;-----------------------Info in left panel--------------------------------------------

;Creates panel for info
(define player-info-panel
  (new vertical-panel%
       (parent left-panel)
       ))

;Creates panel for info on player name
(define player-name-panel
  (new horizontal-panel%
       (parent player-info-panel)
       [alignment '(center top)]
       ))

;Shows "Player name: "
(define player-name-message
  (new message%
       (parent player-name-panel)
       (min-width 50)
       (min-height 15)
       (label "Player name:")
       ))

;Shows current-player-name
(define current-name-message
  (new message%
       (parent player-name-panel)
       (min-width 75)
       (min-height 15)
       (label "none")
       ))

;Creates panel for info on score
(define score-panel
    (new horizontal-panel%
       (parent player-info-panel)
       [alignment '(center top)]
       ))

;Message that show "Score:"
(define score-message
  (new message%
       (parent score-panel)
       (min-width 50)
       (min-height 15)
       (label "Score:")
       ))

;Shows current-score
(define current-score-message
  (new message%
       (parent score-panel)
       (min-width 75)
       (min-height 15)
       (label (number->string 0))
       ))

;Panel for current-level
(define level-panel
  (new horizontal-panel%
       (parent player-info-panel)
       [alignment '(center top)]
       ))

;Message that show "Level"
(define level-message
  (new message%
       (parent level-panel)
       (min-width 50)
       (min-height 15)
       (label "Level:")
       ))

;Message that shows current-level
(define current-level-message
  (new message%
       (parent level-panel)
       (min-width 75)
       (min-height 15)
       (label "1")
       ))

;Creates panel for next-shape
(define next-shape-panel
  (new horizontal-panel%
       (parent player-info-panel)
       [alignment '(center center)]
       ))
;Fills out space so that the next-shape canvas stays symmetric
(define next-shape-panel-fill-out-1
  (new vertical-panel%
       (parent next-shape-panel)
       (min-width 25)
       ))

;Panel for next-shape-canvas
(define next-shape-canvas-panel
  (new vertical-panel%
       (parent next-shape-panel)
       [alignment '(center bottom)]
       ))
;Prints "Next shape"
(new message%
     (parent next-shape-canvas-panel)
     (label "Next shape"))

;Canvas for next-shape
(define next-shape-canvas
  (new canvas%
       (parent next-shape-canvas-panel)
       (min-height 100)
       (min-width 100)
       (paint-callback
        (lambda (canvas dc)
            ;Draws the next shape
            (define (draw-next-shape)
              (cond
                ((eq? (send *player* get-next-shape-color) 0) (send dc set-brush "green" 'solid))
                ((eq? (send *player* get-next-shape-color) 1) (send dc set-brush "red" 'solid))
                ((eq? (send *player* get-next-shape-color) 2) (send dc set-brush "yellow" 'solid))
                ((eq? (send *player* get-next-shape-color) 3) (send dc set-brush "blue" 'solid))
                ((eq? (send *player* get-next-shape-color) 4) (send dc set-brush "purple" 'solid))
                ((eq? (send *player* get-next-shape-color) 5) (send dc set-brush "orange" 'solid))
                )
              (cond
                ;Draws L-shape
                ((eq? (send *player* get-next-shape) 0)
                 (send dc draw-rectangle (* 25 1) (* 25 1) 25 25)
                 (send dc draw-rectangle (* 25 1) (* 25 2) 25 25)
                 (send dc draw-rectangle (* 25 1) (* 25 3) 25 25)
                 (send dc draw-rectangle (* 25 2) (* 25 3) 25 25))
                ;Draws I-shape
                ((eq? (send *player* get-next-shape) 1)
                 (send dc draw-rectangle (* 25 1) (* 25 0) 25 25)
                 (send dc draw-rectangle (* 25 1) (* 25 1) 25 25)
                 (send dc draw-rectangle (* 25 1) (* 25 2) 25 25)
                 (send dc draw-rectangle (* 25 1) (* 25 3) 25 25))
                ;Draws O-shape
                ((eq? (send *player* get-next-shape) 2)
                 (send dc draw-rectangle (* 25 1) (* 25 1) 25 25)
                 (send dc draw-rectangle (* 25 2) (* 25 1) 25 25)
                 (send dc draw-rectangle (* 25 1) (* 25 2) 25 25)
                 (send dc draw-rectangle (* 25 2) (* 25 2) 25 25))
                ;Draws T-shape
                ((eq? (send *player* get-next-shape) 3)
                 (send dc draw-rectangle (* 25 1) (* 25 1) 25 25)
                 (send dc draw-rectangle (* 25 2) (* 25 1) 25 25)
                 (send dc draw-rectangle (* 25 3) (* 25 1) 25 25)
                 (send dc draw-rectangle (* 25 2) (* 25 2) 25 25))
                
                ((eq? (send *player* get-next-shape) 4)
                 (send dc draw-rectangle (* 25 1) (* 25 1) 25 25)
                 (send dc draw-rectangle (* 25 1) (* 25 2) 25 25)
                 (send dc draw-rectangle (* 25 2) (* 25 2) 25 25)
                 (send dc draw-rectangle (* 25 2) (* 25 3) 25 25))
                ((eq? (send *player* get-next-shape) 5)
                 (send dc draw-rectangle (* 25 2) (* 25 1) 25 25)
                 (send dc draw-rectangle (* 25 2) (* 25 2) 25 25)
                 (send dc draw-rectangle (* 25 1) (* 25 2) 25 25)
                 (send dc draw-rectangle (* 25 1) (* 25 3) 25 25))
                ((eq? (send *player* get-next-shape) 6)
                 (send dc draw-rectangle (* 25 2) (* 25 1) 25 25)
                 (send dc draw-rectangle (* 25 2) (* 25 2) 25 25)
                 (send dc draw-rectangle (* 25 2) (* 25 3) 25 25)
                 (send dc draw-rectangle (* 25 1) (* 25 3) 25 25))
                ))
          (send dc clear)
          (draw-next-shape))
        )))

;Fills out space so that the next-shape canvas stays symmetric
(define next-shape-panel-fill-out-2
  (new vertical-panel%
       (parent next-shape-panel)
       (min-width 25)
       ))

;Fill-out panel in left field so that everything written stays in the right position
(define fill-out-panel
  (new panel%
       (parent player-info-panel)
       (min-height 320)
       ))
       
       

;-----------------------------KEY-PRESS-EVENTS---------------------------------------

(define (on key)
  (if (eq? (send *player* get-mode) 'play)
  (cond 
    ((eq? 'up (send key get-key-code)) (send *player* rotate-shape))
    ((eq? 'left (send key get-key-code)) (send *player* left))
    ((eq? 'right (send key get-key-code)) (send *player* right))
    ((eq? #\space (send key get-key-code)) (send *player* fall-down-all-way))
    ((eq? 'down (send key get-key-code)) (send *player* drop-shape-down))
    ))
  (cond 
    ((eq? #\p (send key get-key-code)) (send *player* pause))
    ((eq? #\r (send key get-key-code)) (send *player* pause))))

;Creates the class for gameboard-canvas
(define gameboard-canvas% 
  (class canvas%
    (define/override (on-char key-press-event)   
      (on key-press-event))
    
    (super-new)))

;------------------------------------------------------------------------------

;Defines background of gameboard-canvas
(define background
  (make-object bitmap% "Bakgrund/Background-tree.jpg"))


;-----------------------------DRAWING-LOOP---------------------------------------

;Canvas that is the gameboard and draws the stones
(define gameboard-canvas 
  (instantiate gameboard-canvas%
    (main-panel)
    (min-width 368)
    (min-height (send background get-height))
    
    ;Draws the stones
    (paint-callback
     (lambda (canvas dc)
          (define (row-loop row) ;Loops row-numbers until all rows are drawn
            (define (col-loop col) ;Loops col-numbers untill cols are drawn
              (cond
                ((= col 12) (row-loop (+ row 1))) ;10-columns
                ((not (eq? (send *gameboard* get-position row col) 0))
                 (send dc set-brush (send (send *gameboard* get-position row col) get-stone-color) 'solid)
                 (send dc draw-rectangle (+ 50 (* (- col 1) 25)) (+ 75 (* (- row 2) 25)) 25 25)
                 (col-loop (+ col 1)))
                (else (col-loop (+ col 1)))
                ))
            (if (<= row 23)(col-loop 2))) ;20 rows
       (send dc clear)
       (send dc draw-bitmap background 0 0)
       (row-loop 4)))))
          
