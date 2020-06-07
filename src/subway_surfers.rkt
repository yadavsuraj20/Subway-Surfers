#lang racket
(require  pict3d
          pict3d/universe
          2htdp/universe
          racket/gui
          compatibility/mlist
          rsound)

;pict3d for making 3D objects
;pict3d/univerese for the motion the objects
;2htdp/univerese for handling the key events
;gui for the frames and canvas
;compatibility/mlist for mutable lists
;rsound for the sound
;(play (rs-read "ChillingMusic.wav"))

(define name "")
(define score 0)
(define high-score 0)
(define coin-count 0)
(define total-coins 0)

;welcomeframe object of frame%
(define welcomeframe (new frame%
                          [label "Subway Surfers"]
                          [width 1300]
                          [height 720]))

;objects of message% in welcomeframe
(define msg (new message% [parent welcomeframe] 
                 [label "Hope you enjoy playing!!"]
                 [font (make-object font% 20 'roman)]))

(define msg7 (new message% [parent welcomeframe] 
                  [label "                   "]
                  [font (make-object font% 20 'roman)]))

; a object of text-field% that takes the name of the user
(define a (new text-field% [parent welcomeframe]
               [label "Enter Your Name"]
               [font (make-object font% 20 'roman)]
               [vert-margin 100]
               [init-value ""]
               [min-width 500]
               [stretchable-width #f]
               [stretchable-height #f]
               (callback (lambda (textbox event)
                           (begin (set! name (send textbox get-value)))))))

;objects of button% in welcomeframe
(new button% [parent welcomeframe] 
     [label "Play Game"]
     [font (make-object font% 25 'roman)]
     [callback (lambda (button event)
                 (if (eq? name "") (send enter-name show #t)
                     (begin (send welcomeframe show #f)
                            (if(send welcomeframe is-fullscreened?)
                               (begin (send secondframe show #t) (send secondframe fullscreen secondframe))
                               (send secondframe show #t)
                               ))))])

(new button% [parent welcomeframe]
     [label "How to play?"]
     [font (make-object font% 25 'roman)]
     [callback (lambda (button event)
                 (send instruction-frame show #t))])

(new button% [parent welcomeframe] 
     [label "Full Screen"]
     [font (make-object font% 25 'roman)]
     [callback (lambda (button event)
                 (if (send welcomeframe is-fullscreened?)
                     (send msg7 set-label "Already there!!")
                     (begin (send msg7 set-label " ") (send welcomeframe fullscreen #t))))])

(new button% [parent welcomeframe] 
     [label "Exit Full Screen"]
     [font (make-object font% 25 'roman)]
     [callback (lambda (button event)
                 (if(send welcomeframe is-fullscreened?)
                    (begin (send msg7 set-label " ") (send welcomeframe fullscreen #f))
                    (send msg7 set-label "Already there!!")
                    ))])

(new button% [parent welcomeframe] 
     [label "Exit"]
     [font (make-object font% 25 'roman)]
     [callback (lambda (button event)
                 (begin (send welcomeframe show #f) (displayln "Thanks for playing")
                        (displayln "Have a Nice Day")))])


;instruction-frame
(define instruction-frame (new frame%
                               [label "Instructions"]
                               ;[width 1500]
                               ;[height 700]
                               [x 450]
                               [y 225]
                               [stretchable-width #f]
                               [stretchable-height #f]))

(new message% [parent instruction-frame]
     [label "INSTRUCTIONS  "]
     [font (make-object font% 25 'roman)])

(new message% [parent instruction-frame]
     [label ""]
     [font (make-object font% 20 'roman)])

(new message% [parent instruction-frame]
     [label "Up arrow key - Jump "]
     [font (make-object font% 20 'roman)])

(new message% [parent instruction-frame]
     [label "Down arrow key - Slide on the ground "]
     [font (make-object font% 20 'roman)])

(new message% [parent instruction-frame]
     [label "Left arrow key - Move to the left row "]
     [font (make-object font% 20 'roman)])

(new message% [parent instruction-frame]
     [label "Right arrow key - Move to the right row "]
     [font (make-object font% 20 'roman)])

(new message% [parent instruction-frame]
     [label ""]
     [font (make-object font% 20 'roman)])

(new button% [parent instruction-frame]
     [label "Back"]
     [font (make-object font% 20 'roman)]
     [callback (lambda (button event)
                 (send instruction-frame show #f))])

; enter-name frame will be displayed when no name is entered
(define enter-name (new frame%
                        [label ""]
                        [width 400]
                        [height 350]
                        [x 500]
                        [y 325]
                        [stretchable-width #f]
                        [stretchable-height #f]))

(define msg-name (new message% [parent enter-name]
                      [label "Please Enter Your name"]
                      [font (make-object font% 20 'roman)]))

(new button% [parent enter-name]
     [label "Back"]
     [font (make-object font% 20 'roman)]
     [callback (lambda (button event)
                 (send enter-name show #f))])

;secondframe object of frame%
(define secondframe (new frame%
                         [label ""]
                         [width 1300]
                         [height 900]))

(new button% [parent secondframe]
     [label "Back"]
     [font (make-object font% 20 'roman)]
     [callback (lambda (button event)
                 (begin (send secondframe show #f)
                        (send welcomeframe show #t)))])

(new message% [parent secondframe] 
     [label "All the Best"]
     [font (make-object font% 20 'roman)])

;menu-click class inherited from canvas%
(define (menu-click frame)
  (class canvas%
    (define/override (on-event mouse-event)
      (cond [(eq? (send mouse-event get-event-type) 'left-down) 
             (> (send mouse-event get-x) 0) (begin (send secondframe show #f) (game))
             ]))
    (super-new [parent secondframe])))

; object of menu-click class
(new (menu-click secondframe)
     [paint-callback 
      (lambda (canvas1 dc)    
        (if(send welcomeframe is-fullscreened?)
           (send dc draw-bitmap
                 (make-object bitmap% "subwaysurfers.jpg")                                
                 40 0)
           (send dc draw-bitmap
                 (make-object bitmap% "subwaysurfers.jpg")                                
                 10 0))
        )])

;                                                     GAME
(define (game)
  ;;;;;;;;;;;;;;;;;;;;;;;;HELPER FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define mnull (mcdr (mlist 0)))
  (set! coin-count 0)

  (define (take li n ans)
    (cond
      [(= n 0) ans]
      [else (take (mcdr li) (- n 1) (mappend ans (mlist (mcar li))))]))
  
  (define temp 0)
  ;;;;;;;;;;;;
  (define (m0 n)
    (cond
      [(= n 0) '()]
      [else (mappend (mlist 0) (m0 (- n 1)))]
      ))
  (define (last li)
    (cond
      [(null? (mcdr li)) (mcar li)]
      [else (last (mcdr li))]))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;COIN GENERATOR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define coinlist '((0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0)
                             (1 0 0) (0 1 0) (0 0 1) (2 0 0) (0 2 0) (0 0 2) (3 0 0) (0 3 0) (0 0 3) (1 0 2) (3 0 1) (2 0 3)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (define groundcoins (mlist 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 0 0))
  (define aircoins (mlist 0 0 0 0 0 2 0 2 0 2 0 2 0 2 0 2 0))
  (define mixture (mlist 0 0 0 1 0 1 0 2 0 2 0 2 0 1 0 1 0))
  (define nocoins (mlist 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (define jetpack (mlist 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define coinrow1 (m0 100))
  (define coinrow2 (m0 100))
  (define coinrow3 (m0 100))
  (define (choosejet) (list-ref '((4 0 0) (0 4 0) (0 0 4)) (random 3)))
  (define (add-coin)
    (define (jet-addition)
      (if (= (random 10) 0) #t #f))
    (let*
        (  [option-chosen (+ (random 24) 1)]
           [final-list (if (jet-addition) (choosejet) (list-ref coinlist (- option-chosen 1)))] ;;1
           [pos1 (car final-list)]
           [pos2 (cadr final-list)]
           [pos3 (caddr final-list)])
      (begin
        (cond
          [(= pos1 0) (set! coinrow1 (mappend coinrow1 nocoins))]
          [(= pos1 1) (set! coinrow1 (mappend coinrow1 groundcoins))]
          [(= pos1 2) (set! coinrow1 (mappend coinrow1 aircoins))]
          [(= pos1 3) (set! coinrow1 (mappend coinrow1 mixture))]
          [(= pos2 4) (set! coinrow1 (mappend coinrow1 jetpack))])

        (cond
          [(= pos2 0) (set! coinrow2 (mappend coinrow2 nocoins))]
          [(= pos2 1) (set! coinrow2 (mappend coinrow2 groundcoins))]
          [(= pos2 2) (set! coinrow2 (mappend coinrow2 aircoins))]
          [(= pos2 3) (set! coinrow2 (mappend coinrow2 mixture))]
          [(= pos2 4) (set! coinrow2 (mappend coinrow2 jetpack))])

        (cond
          [(= pos3 0) (set! coinrow3 (mappend coinrow3 nocoins))]
          [(= pos3 1) (set! coinrow3 (mappend coinrow3 groundcoins))]
          [(= pos3 2) (set! coinrow3 (mappend coinrow3 aircoins))]
          [(= pos3 3) (set! coinrow3 (mappend coinrow3 mixture))]
          [(= pos2 4) (set! coinrow3 (mappend coinrow3 jetpack))])

        #t)))
  (define (Manipulatecoin)
    (cond
      [(<= 100 (mlength coinrow1)) (begin (set! coinrow1 (mcdr coinrow1))
                                          (set! coinrow2 (mcdr coinrow2))
                                          (set! coinrow3 (mcdr coinrow3))
                                          #t)]
      [else (add-coin)]))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RUNNER PARAMETERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define hdecr .25)  
  (define Runner-height 0)
  (define Runner-row 2)
  (define stop-var-x 0)
  (define stop-var-y 0)
  (define (next-step) (cond
                        [(= 1 Runner-row) (mlist-ref row1 8)]
                        [(= 2 Runner-row) (mlist-ref row2 8)]     ;;;;;;;;;;;;;;;;;;;problem with choosing last
                        [(= 3 Runner-row) (mlist-ref row3 8)]))
  (define (next-coin) (cond
                        [(= 1 Runner-row) (mlist-ref coinrow1 10)]
                        [(= 2 Runner-row) (mlist-ref coinrow2 10)]     ;;;;;;;;;;;;;;;;;;;problem with choosing last
                        [(= 3 Runner-row) (mlist-ref coinrow3 10)]))

  (define (current-coin)
    (cond
      [(= (next-coin) 1) (cond
                           [(= (next-step) 2) 1.5]
                           [(= (next-step) 3) 2.5]
                           [else 0.5])]
      [(= (next-coin) 2) (cond
                           [(= (next-step) 2) 2.5]
                           [(= (next-step) 3) 3.5]
                           [else 1.5])]
      [(= (next-coin) 3) (cond
                           [(= (next-step) 2) 1.5]
                           [(= (next-step) 3) 2.5]
                           [else 0.5])]
      [else #f]))

  (define (coin-collect) (let*
                             ([tt  (current-coin)])
                           (if (and (number? tt) (and (< Runner-height (+ 0.4 tt)) (> Runner-height (- tt 0.4))))
                               (begin
                                 (if (= (next-coin) 3) (set! fly 200) #t)
                                 (delete-coin))
                               (set! temp 0))))

  ;  (define (delete-coin)
  ;    (begin (play (rs-overlay click-1 click-2)) (set! coin-count (+ 1 coin-count)))
  ;    (cond
  ;      [(= 1 Runner-row) (set! coinrow1 (mappend (m0 10) (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr coinrow1))))))))))))]
  ;      [(= 2 Runner-row) (set! coinrow2 (mappend (m0 10) (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr coinrow2))))))))))))]     ;;;;;;;;;;;;;;;;;;;problem with choosing last
  ;      [(= 3 Runner-row) (set! coinrow3 (mappend (m0 10) (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr coinrow3))))))))))))]))

  (define (delete-coin)
    (begin (play (rs-overlay click-1 click-2)) (set! coin-count (+ 1 coin-count)))                                              ;;;;;;
    (cond                                                                                                                       ;;;;;;
      [(= 1 Runner-row) (let*                                                                                                   ;;;;;;
                            ([tv (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr coinrow1)))))))))))])        ;;;;;;
                          (set! coinrow1 (mappend (take coinrow1 10 mnull) (mlist 0) tv)))]                                     ;;;;;;
      [(= 2 Runner-row) (let*                                                                                                   ;;;;;;vanishing coins
                            ([tv (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr coinrow2)))))))))))])        ;;;;;;
                          (set! coinrow2 (mappend (take coinrow2 10 mnull) (mlist 0) tv)))]                                     ;;;;;; 
      [(= 3 Runner-row) (let*                                                                                                   ;;;;;;
                            ([tv (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr coinrow3)))))))))))])        ;;;;;;
                          (set! coinrow3 (mappend (take coinrow3 10 mnull) (mlist 0) tv)))]))                                   ;;;;;; 

  
    
  (define (change-runner)            
    (if (not (= slide 0)) (begin (if (= slide 8) (set! z-var (- z-var 0.2)) (set! z-var z-var)) (set! slide (- slide 1)))
        (begin (if (> i 2) (set! z-var (min 4 (+ z-var 0.25))) #t)
               (if (= i 0)
                   (if (= (next-step) 0) (changeheight Runner-height 0.5)
                       (if (=  (next-step) 1)
                           (changeheight Runner-height 1)                                                  ;;;step2
                           (changeheight Runner-height (- (next-step) 0.5))))                              ;;;Changing Runner Height from surrounding conditions
                   (begin (set! i (- i 1)) (= Runner-height z-var))))))                                    
  
 
  
  (define (change-row-by-cam)                                                                       ;;;step3
    (set! Runner-row (+ 2 (/ x-var 1.5))))                                                          ;;;To do row calculaion for coins
  
  (define (change-runner-by-cam)                                                                    ;;;step1      
    (set! Runner-height z-var))                                                                     ;;;To implement change in Runner Parameters from camera 
  
  (define (changeheight x y)
    (begin (set! stop-var-x x) (set! stop-var-y y)
           (cond
             [(and (= y 1) (not (= x 0.5))) (set! z-var z-var)]
             [(> x y) (begin (set! z-var (- z-var hdecr)) (set! Runner-height (- Runner-height hdecr))) (= Runner-height z-var)]
             )) )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Flying Jetpack:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (flying-man)             
    (if (not (= fly 0)) (begin (set! z-var (min 6 (+ z-var 0.5))) (set! fly (- fly 1))) (set! fly 0)))

  


  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define row1 (m0 100))
  (define row2 (m0 100))
  (define row3 (m0 100))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define slide-obs (mlist 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (define block-obs (mlist 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))
  (define train-obs (mlist 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3))
  (define no-obs    (mlist 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (struct rowstruct (r1 r2 r3) #:transparent)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (add-obstacle li)
    (let*
        (  [option-chosen (+ (random (length li)) 1)]
           [final-list (list-ref li (- option-chosen 1))];;1
           [pos1 (car final-list)]
           [pos2 (cadr final-list)]
           [pos3 (caddr final-list)])
      (begin
        (cond
          [(= pos1 0) (set! row1 (mappend row1 no-obs))]
          [(= pos1 1) (set! row1 (mappend row1 slide-obs))]
          [(= pos1 2) (set! row1 (mappend row1 block-obs))]
          [(= pos1 3) (set! row1 (mappend row1 train-obs))])

        (cond                                                      ;;;;;;;;;;;;;;;obstacle adding
          [(= pos2 0) (set! row2 (mappend row2 no-obs))]
          [(= pos2 1) (set! row2 (mappend row2 slide-obs))]
          [(= pos2 2) (set! row2 (mappend row2 block-obs))]
          [(= pos2 3) (set! row2 (mappend row2 train-obs))])

        (cond
          [(= pos3 0) (set! row3 (mappend row3 no-obs))]
          [(= pos3 1) (set! row3 (mappend row3 slide-obs))]
          [(= pos3 2) (set! row3 (mappend row3 block-obs))]
          [(= pos3 3) (set! row3 (mappend row3 train-obs))])

        (set! ROW (rowstruct row1 row2 row3))
        #t)))
  

  (define ROW (rowstruct row1 row2 row3))
  
  (define (Possible-outcome v1 v2 v3)
    (define init (if (or (= v3 2) (= v3 3)) '((3) (2) (1) (0)) '((2) (1) (0))))                      ;;;;;;;;;;;;;;Zero Layer
    (define (g x)
      (cond
        [(or (= v2 0) (= v2 1))   (let*
                                      ([te (car x)])                                                 ;;;;;;;;;;;;;;First layer
                                    (cond
                                      [(= (car x) 3) (list (list 0 3) (list 1 3) (list 2 3))]
                                      [else (list (list 0 te) (list 1 te) (list 2 te) (list 3 te))]))]                                            ;;;;;;possible outcomes
        [(= v2 2) (let*
                      ([te (car x)])
                    (list (list 0 te) (list 1 te) (list 2 te) (list 3 te)))]
        [(= v2 3) (let*
                      ([te (car x)])                                            
                    (list (list 0 te) (list 1 te) (list 2 te)))]))
    (define (h x)
      (cond
        [(or (= v3 0) (= v3 1))   (let*
                                      ([te (car x)]
                                       )                                                            ;;;;;;;;;;;;;;Second Layer
                                    (cond
                                      [(= (car x) 3) (list (append (list 0) x) (append (list 1) x) (append (list 2) x))]
                                      [else (list (append (list 0) x) (append (list 1) x) (append (list 2) x) (append (list 3) x))]))]
        [(or (= v3 2) (= v3 3))   (list (append (list 0) x) (append (list 0) x) (append (list 0) x) (append (list 0) x))]
        ))
    (append* (map h (append* (map g init)))))
  
  (define (Change x)
    (add-obstacle (Possible-outcome (last row1) (last row2) (last row3))))
  
  (define (Manipulate)
    (cond
      [(<= 100 (mlength row1)) (begin (set! row1 (mcdr row1))
                                      (set! row2 (mcdr row2))
                                      (set! row3 (mcdr row3))                           ;;;;;;;;;;;;;Changing Row-Lengths (Reducing and Increasing again)
                                      (set! ROW (rowstruct row1 row2 row3))
                                      #t)]
      [else (Change ROW)]))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define endframe (new frame%
                        [label "End frame"]
                        [width 1300]
                        [height 720]
                        [stretchable-width #f]
                        [stretchable-height #f]))

  (new message% [parent endframe]
       [label "Hope you enjoyed playing"]
       [font (make-object font% 20 'roman)])
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define pause-var 0)
  (define x-var 0)
  (define z-var 0.5)
  (define i 0)
  (define slide 0)
  (define fly 0)
  (current-material (material #:ambient 0.025      ;material properties, ambient is useful
                              #:diffuse 0.39
                              #:specular 0.6
                              #:roughness 0.2))

  (current-pict3d-background (rgba "light blue" 010))

  (define (list->blocks l x y z)  ;function which converts list to pict3d blocks
    (define (block)
      (cond [(= (mcar l) 2) (set-color (rectangle (pos x y z) (pos (+ x 1) (- y 0.2) (+ z 1))) (rgba "green"))]
            [(= (mcar l) 1)
             (set-color (combine (rectangle (pos x y (+ z 0.85)) (pos (+ x 1) (- y 0.2) (+ z 2)))
                                 (rectangle (pos x y z) (pos (+ x 0.1) (- y 0.2) (+ z 1)))
                                 (rectangle (pos (+ x 0.9) y z) (pos (+ x 1) (- y 0.2) (+ z 1))))
                        (rgba "blue"))]
            [else (set-color (rectangle (pos x y z) (pos (+ x 1) (- y 0.2) (+ z 2))) (rgba "orange"))]))
    (cond [(null? (mcdr l)) (block)]
          [(= (mcar l) 0) (list->blocks (mcdr l) x (- y 0.2) z)]
          [else (combine (block) (list->blocks (mcdr l) x (- y 0.2) z))]))

  (define (realtrain l x)
    (list->blocks l x 0 0))

  (define (3-trains l1 l2 l3)
    (combine (realtrain l1 -2)
             (realtrain l2 -0.5)
             (realtrain l3 1)))
  
  (define (lights)                                                   ;lights placed
    (combine (light (pos 0 -15 3) (emitted "White" 5))
             (light (pos 0 -10 3) (emitted "White" 5))
             (light (pos 0 -5 3) (emitted "White" 5))
             (light (pos 0 0 3) (emitted "White" 5))
             (light (pos 0 -20 8) (emitted "White" 5))
             (light (pos 0 -10 8) (emitted "White" 5))
             (light (pos 0 0 8) (emitted "White" 5))))
  
  (define (road-and-walls)                                           ;background
    (combine (rectangle (pos -2 2 0) (pos 2 -75 -0.1))
             (with-color (rgba "Thistle" 1) (rectangle (pos -2 2 0) (pos -2.2 -75 1.5)))
             (with-color (rgba "Thistle" 1) (rectangle (pos 2 2 0) (pos 2.2 -75 1.5)))
             (with-color (rgba "red" 0.5) (rectangle (pos -2 2 1.5) (pos -2.2 -75 5)))
             (with-color (rgba "red" 0.5) (rectangle (pos 2 2 1.5) (pos 2.2 -75 5)))
             (with-color (rgba "red" 0.5) (rectangle (pos -2 2 5) (pos 2 -75 5.2)))))

  (define obj3 0)
  (define obj2 0)
  (define obj1 0)
  (define obj 0)
  (define z-obj 0)

  (define (on-draw s n t)              ;function which updates the current world continuously
    (define y 0)
    (cond [(stop-state? s n t) (begin (set! score (exact-floor (/ t 10)))
                                      (new message% [parent endframe]
                                           [label "GAME OVER"]
                                           [font (make-object font% 20 'roman)])
                                      (new message% [parent endframe]
                                           [label (string-append "Your Score : " (~a score))]
                                           [font (make-object font% 20 'roman)])
                                      (new message% [parent endframe]
                                           [label (string-append "Coins collected : " (~a coin-count))]
                                           [font (make-object font% 20 'roman)])
                                      (set! total-coins (+ total-coins coin-count))
                                      (new message% [parent endframe]
                                           [label (string-append "Total coins : " (~a total-coins))]
                                           [font (make-object font% 20 'roman)])
                                      (cond [(> score high-score) (set! high-score score)])
                                      (new message% [parent endframe]
                                           [label (string-append "High Score : " (~a high-score))]
                                           [font (make-object font% 20 'roman)])
                                      (new button% [parent endframe]
                                           [label "Play Again"]
                                           [font (make-object font% 20 'roman)]
                                           [callback (lambda (button event)
                                                       (begin (send endframe show #f) (game)))])
                                      (new button% [parent endframe]
                                           [label "Exit"]
                                           [font (make-object font% 20 'roman)]
                                           [callback (lambda (button event)
                                                       (begin (exit) (send endframe show #f)))])
                                      (send endframe show #t))])
    (begin (Manipulate)                    ;functions which should be called in each iteration
           (Manipulatecoin)
           (change-runner-by-cam)
           (change-runner)
           (change-row-by-cam)
           (coin-collect)
           (flying-man)
           (set! obj3 (mlist-ref row3 9))
           (set! obj2 (mlist-ref row2 9))
           (set! obj1 (mlist-ref row1 9))
           (set! obj (cond [(= x-var 1.5) obj3]
                           [(= x-var 0) obj2]
                           [else obj1]))
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (define (coins x y z)                                ;the shape of the rotating coins
             (with-color (rgba "yellow" 1)
               (rotate/center (move-z (move-x (move-y (rotate/center (cylinder (pos -0.2 0 0) (pos 0.2 (+ 0 0.4) 0.05))
                                                                     (dir 1 0 0) 90 (pos 0 0.15 0.025)) y) x) z)
                              (dir 0 0 1) (/ t 2) (pos x (+ y 0.2) (+ z 0.025)))))
           (define (star x y z)                                       ; the jetpack coin
             (with-color (rgba "red" 1) (cylinder (pos (+ x -0.2) (+ y 0) (+ z 0)) (pos (+ x 0.2) (+ y 0.4) (+ z 0.5)))))
           
           (define (list->coins l-coins l-objs x y)               ;function which converts list of coins to the 3d coins
             (define (coin) (cond [(null? (mcdr l-objs)) ]
                                  [(or (= (mcar l-objs) 0) (= (mcar l-objs) 1)) (cond [(= (mcar l-coins) 1) (coins x y 0.5)]
                                                                                      [(= (mcar l-coins) 2) (coins x y 1.5)]
                                                                                      [else (star x y 0.5)])]
                                  [(= (mcar l-objs) 2) (cond [(= (mcar l-coins) 1) (coins x y 1.5)]
                                                             [(= (mcar l-coins) 2) (coins x y 2.5)]
                                                             [else (star x y 1.5)])]
                                  [else (cond [(= (mcar l-coins) 1) (coins x y 2.5)]
                                              [(= (mcar l-coins) 2) (coins x y 3.5)]
                                              [else (star x y 2.5)])]))
             (cond [(or (null? (mcdr l-coins)) (null? (mcdr l-objs))) (sphere origin 0.5)]
                   [(= (mcar l-coins) 0) (list->coins (mcdr l-coins) (mcdr l-objs) x (- y 0.2))]
                   [else (combine (coin) (list->coins (mcdr l-coins) (mcdr l-objs) x (- y 0.2)))]))
           
           (combine (move-y (3-trains row1 row2 row3)              ;this is the pict3d world of obstacles
                            (begin (set! y (- y 0.2)) y))
                    (combine (list->coins coinrow1 row1 -1.5 0)
                             (list->coins coinrow2 row2 0 0)
                             (list->coins coinrow3 row3 1.5 0))
                    (cond [(and (= slide 0) (= fly 0)) (with-color (rgba "turquoise" 1)
                                                         (ellipsoid (pos (- x-var 0.25) -2.25 (- z-var 0.5))
                                                                    (pos (+ x-var 0.25) -1.75 (+ z-var 0.5))))]
                          [(= fly 0) (with-color (rgba "turquoise" 1)
                                       (ellipsoid (pos (- x-var 0.25) -2.25 (- z-var 0.3))
                                                  (pos (+ x-var 0.25) -1.75 (+ z-var 0.3))))]
                          [else (with-color (rgba "turquoise")
                                  (ellipsoid (pos (- x-var 0.25) -2.5 (- z-var 0.25))
                                             (pos (+ x-var 0.25) -1.5 (+ z-var 0.25))))])
                    (road-and-walls)
                    (lights)
                    (basis 'camera (point-at (pos x-var 1 (+ z-var 2)) (pos x-var -1.1 (+ z-var 2)))))))
  
  (define (check-left)           ;checks if there is a space to go to the left row
    (cond [(= x-var 1.5) #t]
          [(= x-var 0) (cond [(>= z-var 2.5) #t]
                             [(>= z-var 1.5) (if (or (= obj3 0) (= obj3 2)) #t #f)]
                             [else (if (= obj3 0) #t #f)])]
          [else (cond [(>= z-var 2.5) #t]
                      [(>= z-var 1.5) (if (or (= obj2 0) (= obj2 2)) #t #f)]
                      [else (if (= obj2 0) #t #f)])]))

  (define (check-right)               ;checks if there is a space to go to the right row 
    (cond [(= x-var -1.5) #t]
          [(= x-var 0) (cond [(>= z-var 2.5) #t]
                             [(>= z-var 1.5) (if (or (= obj1 0) (= obj1 2)) #t #f)]
                             [else (if (= obj1 0) #t #f)])]
          [else (cond [(>= z-var 2.5) #t]
                      [(>= z-var 1.5) (if (or (= obj2 0) (= obj2 2)) #t #f)]
                      [else (if (= obj2 0) #t #f)])]))
  
  (define (check-down)                    ;checks whether there is an object at the bottom so that the ball can slide
    (cond [(= z-var 0.5) (set! slide 8)]
          [(= obj 2) (if (= z-var 1.5) (set! slide 8) (set! slide 0))]
          [else (if (= z-var 2.5) (set! slide 8) (set! slide 0))]))
  
  (define (left-right s n t k)                ;It takes the key events and updates accordingly
    (cond [(key=? k "left") (begin (set! slide 0) (if (= 0.3 z-var) (set! z-var 0.5) (set! z-var z-var))
                                   (if (check-left) (set! x-var (min 1.5 (+ x-var 1.5))) (set! x-var x-var)))] 

          [(key=? k "right") (begin (set! slide 0) (if (= 0.3 z-var) (set! z-var 0.5) (set! z-var z-var))
                                    (if (check-right) (set! x-var (max (- x-var 1.5) -1.5)) (set! x-var x-var)))]

          [(key=? k "up") (begin
                            ;(display i)
                            (set! slide 0)
                            (if (= 0.3 z-var) (set! z-var 0.5) (set! z-var z-var))
                            (if (= i 0)
                                (if (= stop-var-x stop-var-y)
                                    (begin (set! i 6)
                                           ;(displayln i)
                                           (set! slide 0)
                                           (set! stop-var-x (min 3 (+ z-var 0.3)))
                                           (set! z-var (min 3 (+ z-var 0.3))))
                                    (set! z-var z-var))
                                (set! z-var z-var))
                            (set! Runner-height z-var))]

          [(key=? k "down") (check-down)];(max 0.5 (- z-var 1))))]

          [(key=? k " ") (set! pause-var (- 1 pause-var))]

        ; [(key=? k "f") (set! fly 200)]
          ))

  ;(define (stop-state? s n t) (if (= 1 stop-var-y) (not (or (<= 2.5 stop-var-x) (> 0.5 stop-var-x))) (< stop-var-x stop-var-y)))
  
  (define (stop-state? s n t)              ;It tells if the collision between ball and obstacle occurs so that the game ends
    (if (= slide 0) (begin (cond [(= z-var 0.3) (set! z-var 0.5)]
                                 [(= z-var 1.3) (set! z-var 1.5)]
                                 [(= z-var 2.3) (set! z-var 2.5)])
                           (cond [(= obj 0) #f]
                                 [(= obj 1) (and (>= z-var 0.5) (< z-var 2.5))]
                                 [(= obj 2) (< z-var 1.5)]
                                 [else (< z-var 2.5)]))
        (cond [(= obj 0) #f]
              [(= obj 1) (and (>= z-var 0.5) (< z-var 2.3))]
              [(= obj 2) (< z-var 1.3)]
              [else (< z-var 2.3)])))
    
  (define (pause-state? s n t)       ; To pause the game
    (if (= pause-var 1) #t #f))

  (big-bang3d 0                                    ;It creates the new window on which the game runs and updates the world according to the
                                                   ;manipulations done by on-draw pause-state? left-right and stop-state? 
              #:display-mode 'fullscreen
              #:stop-state? stop-state?
              #:name "Subway Surfers"
              #:pause-state? pause-state?
              #:on-draw on-draw
              #:on-key left-right))
(send welcomeframe show #t)