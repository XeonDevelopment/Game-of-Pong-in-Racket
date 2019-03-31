#lang racket
(require 2htdp/universe 2htdp/image) ;;Importing libraries 
(require picturing-programs) ;;Importing FOnts
#|
Welcome to Pong
Game instructions :
up (arrow key) - paddle goes up
down (arrow key) - paddle goes down
space (keyboard) - game is now paused
Restart from run to play again
|#

;;SOB 22
;;Structs Vectors strings

;;Sob 25
;;Define Struct Model
;;Changing numerical values of the whole program

;;Sob 27
;;Replace is a signature of a function
;;Collision
;;OffLimits is another
;;Pause Settings

;;SOB 29
;;IF are Mutable
;;Immatuable ORIGINAL RANK2 

;;BASIC DEFINITONS

#|
Here the board is being defined, it is 1000 by 700 board
1000px is the width and 700 is the height
|#
(define-values (width height) (values 1000 700))
;;Here the ball is being defined as having a radius of 30
;;This ball is what the program is using as a projectile
(define ball (circle 30 "solid" "white"))
;;Here is the left side which is the one being controlled by the user
(define user (rectangle 20 80 "solid" "green"))
;;Here is the right side which is the one being controlled by the computer
(define computer (rectangle 20 80 "solid" "blue"))
;;Front screen with the play button and how to play
(define front-screen (place-image (above/align "middle" (text/font "How to Play" 30 "white" "Gill Sans" 'swiss 'normal 'bold #t) ;;Text Centred in bold Font Swiss
                          (rectangle 0 20 "solid" "black")  ;;Space Between Play and Text
                          (text "W key : Move your paddle go up
                               \nS key : Move yourpaddle go down
                               \nSpacebar key : Pause the game
                               \nClick restart at the end of the game to restart" 20 "white"))
                          500 575
              (place-image (underlay (rectangle 150 80 "outline" "white")   ;;Design of the button
                                            (text/font "Play" 50 "white" "Gill Sans" 'swiss 'normal 'bold #f))
                                  500 400
                            (place-image (bitmap "pong.png")
                                  500 150
                                  (empty-scene 1000 700 "black"))))) ;;Background in black

 
;;ADVANCED SETTINGS 
;;Sets the model that is used throughout the program
(define-struct model (rank score)  #:transparent)
;; The margin for the borders and x-ccordinates for the paddles
(define-values (boundry left right)
  (values 10 5 (- width 5)))
;; Paddle step amount,ball velocity, and pause state
(define-values (step pausesetting)
  (values 10 false))
#|
Here we have how the program actually interacts
The probram has 6 different states
Each one controls a specific part of the program
|#
(define (ball-x rank)
  (list-ref rank 0)) ;;Ball cordinates = X
(define (ball-y rank)
  (list-ref rank 1))  ;;Ball cordinates = y
(define (ball-vec-x rank)
  (list-ref rank 2))
(define (ball-vec-y rank)
  (list-ref rank 3))  ;;Ball Directions
(define (user-y rank)
  (list-ref rank 4))  ;;User Padel
(define (computer-y rank)
  (list-ref rank 5))  ;;AI Padel


;;6 States 

;; This will update  a list at tracker with a new value
(define (replace lst tracker value)
  (append (take lst tracker)
          (list value)
          (list-tail lst (add1 tracker))))

;;Takes in states, and is used to update states
;;Helper Function



;;Simple function to tell its something is within a distance
;;Used again in the next collison function
(define (within1? n target dist)
    (and (>= n (- target dist)) (<= n (+ target dist))))
;;How far apart 


;; Is the ball colliding with a paddle?
(define (collision? rank)
  (define (within? n target dist)
    (and (>= n (- target dist)) (<= n (+ target dist))))
  (or (and (equal? (ball-x rank) 20) 
           (within? (ball-y rank) (user-y rank) 80))
      (and (equal? (ball-x rank) (- width 20))
           (within? (ball-y rank) (computer-y rank) 25))))
;;;This is used to detect if the ball is touchingboth Computer and user paddles using defined states 



;; Did the ball travel past either paddle? check botton
(define (off-limits? rank)
  (or (>= boundry (ball-x rank))
      (<= (- width boundry) (ball-x rank))))
;;This detects if you missed the ball (Checking if its game over)(Width, Boundry is pre-defined) (Offlimits is what we are dfining)


;; Move the ball one step
(define (move-ball rank)
  (make-model (replace (replace (model-rank rank) 0 (+ (ball-x (model-rank rank)) (ball-vec-x (model-rank rank))))
           1
           (+ (ball-y (model-rank rank)) (ball-vec-y (model-rank rank))))
              (model-score rank) ))
;;We change numerical values to move the ball
;;EG: Change the x and Y of the ball and directions of movement




;; Next position of the ball
;; if it's paused it will retain the rank

(define (next-ball rank)
  (if pausesetting
      (make-model (model-rank rank) (model-score rank) )
      (move-ball
       (cond
         [(collision? (model-rank rank)) ;;Tests collision
          (make-model (replace (model-rank rank) 2 (* -1 (ball-vec-x (model-rank rank)))) (model-score rank) )] ;;If there is collision use replace statement to change direction of ball
         [(or (equal? (ball-y (model-rank rank)) boundry) ;;Checks the top
              (equal? (ball-y (model-rank rank)) (- height boundry)))
          (make-model (replace (model-rank rank) 3 (* -1 (ball-vec-y (model-rank rank)))) (model-score rank) )] ;;Saves the value of the function
         [else (make-model (model-rank rank) (model-score rank) )]))))


;; Move the left paddle vec pixels
(define (next-player rank vec)
  (define new-y
    (make-model (max 40  (min (- height 25) (+ vec (user-y (model-rank rank))))) (model-score rank) )) ;;
  (if pausesetting
      (make-model (model-rank rank) (model-score rank) )
      (make-model (replace (model-rank rank) 4 (model-rank new-y)) (model-score rank))))

;;This is what draws and pauses the paddle.



;;Fake A.I.
(define (arti-intel rank)
 
  ;; this is where the magic happens. By replacing the rank, it matches the ball's height. (y coordinate)
  (make-model (replace (model-rank rank) 5 (ball-y (model-rank rank))) (model-score rank) ))

;; Pause the game using a toggle
(define (toggle-pause rank)
  (set! pausesetting (not pausesetting))
  rank)

;; Event handlers 
(define (event-handler rank a-key)
  (cond
    [(key=?  a-key "up") (next-player rank (- step))]
    [(key=?  a-key "down") (next-player rank (+ step))]
    [(key=?  a-key " ") (toggle-pause rank)]
    [else rank]))


;; The universe library can move object with ticks. 
;; After every tick ,  It will find new ai position, every time the ball moves
;;Tick handler
;;Used to change the values
(define (world-step mod)
  (cond [(equal? (model-rank mod) original-rank2) ;;Used for draw handler, makes sure the value for the menu doesnt change 
                                                                               ;; and the game doesnt start 
        (make-model original-rank2 (model-score mod))]
        [(and (equal? (ball-x (model-rank mod)) 20)  ;;Checks if you scored a point and adds the point to your score
           (within1? (ball-y (model-rank mod)) (user-y (model-rank mod)) 80))
        (arti-intel (next-ball (make-model (model-rank mod) (+ 1 (model-score mod)))))]
        [(off-limits? (model-rank mod));;Checks if you lost and resets the score
           (make-model (model-rank mod) 0)]
  [else
   (arti-intel (next-ball mod))])) ;; plays the game and moves through helper functions while changing the model

;; These are the initial positions of the sprites
;; The ball will change position, 


;;;;;SOB CHECKLIST 28 
;;Predefined numbers
;;Numbers that can now easily be changed based on how the ball is moving, and how the paddle is moving
(define original-rank 
  (let ([random-vector (λ () (list-ref (list -1 1) (random 2)))]
        [half-width (/ width 2)] 
        [half-height (/ height 2)])
    (list half-width half-height                    ; ball position
          (random-vector) (random-vector)  ; ball direction
          half-height half-height)))                 ; paddle heights
          
          
;;This starts up the game and makes it so it can pull up the meanu
(define original-rank2 '(1 1 1 1 1 1)) ;;Freeze frames

;; Drawing the game area
;; By simply placing the images I can tie everything together. 
;; The only pieces are the ball and 2 paddles.

(define (display-sprites mod)
 (cond [(equal? original-rank2 (model-rank mod)) ;; this is your meanu
        (overlay front-screen ;;front screen which is first frame
        (place-image
         ball 500 500
         (place-image
          user 800 500
          (place-image 
           computer 2000 500
           (add-line(empty-scene width height "black" )  500 0 500 700  (make-pen "white" 5 "solid" "projecting" "bevel"))))))]
      [(off-limits? (model-rank mod))  ;;Checks if you lose the game and then has the restart option as well as game over
        (place-image (bitmap "gameover.png") 500 350 ;;Last frame when you die
        (place-image (text/font "Restart" 40 "white"
             "Gill Sans" 'swiss 'normal 'bold #f) 500 50
        (place-image
         ball -1 -1
         (place-image
          user 0 350
          (place-image 
           computer 1000 350
           (add-line(empty-scene width height "black" )  500 0 500 700  (make-pen "white" 5 "solid" "projecting" "bevel")))))))]
        [else
        (place-image (beside (text "Score: " 50 "white")(text (number->string (model-score mod)) 50 "white")) ;;adds the score to the game
                 300 500
        (place-image
         ball (ball-x (model-rank mod)) (ball-y (model-rank mod)) ;; places the ball based on the numerical x and y values
         (place-image
          user left (user-y (model-rank mod)) ;; places the user paddle based on the numerical x and y values
          (place-image 
           computer right (computer-y (model-rank mod)) ;;places the computer paddle based on the numerical x and y values
           (add-line(empty-scene width height "black" )  500 0 500 700  (make-pen "white" 5 "solid" "projecting" "bevel"))))))])) ;;background
 

;;This is the distance formula that takes 
(define (d-f  x1 y1 x2 y2) ;;distance formula that checks the distance between one x and y value to the other x and y value
  (real->int (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))))
  
  
;;Mouse handler
;;checks where you click
(define (mouser rank x y mouse-event)
  (cond  [(and (string=? mouse-event "button-down") ;;checks if you clicked
              (< (d-f 500 400 x y) 50));; uses the distance formula to check if you clicked in that area
          (make-model original-rank (model-score rank))] ;;play button, this starts the game
         [(and (string=? mouse-event "button-down") ;;checks if you clicked
              (< (d-f 500 50 x y) 40)) ;; restart button, this restarts the game
         (make-model original-rank (model-score rank))]
        [else
         (make-model (model-rank rank) (model-score rank))])) ;; nothing is clicked



;; Creates the frame for the game, and starts when you press run.
(big-bang (make-model '(1 1 1 1 1 1) 0) ;;starting model values, starts at first screen which is menu
          (to-draw display-sprites) ;;draws the the program
          (on-key event-handler) ;; checks what keys you put 
          (on-tick world-step .006) ;; tick handler changes the numerical values
          (on-mouse mouser)) ;; mouse handler for when you click play and restart
