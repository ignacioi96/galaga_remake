;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname galaga_remake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;; Make a spaceship that moves left and right
;                   (when keys "a", "d", "left" or "right" are used)
;                   and does not exit the screen
;                   and shoots lasers (when key " " [space bar] is used)

;; start the world with (main START-WORLD)

;; =================
;; Constants:

(define WIDTH 1000)
(define HEIGHT 700)

(define SPEED-SPACESHIP 20)
(define SPEED-LASER 10)


(define SQUARE-SIZE 5)

(define g (square SQUARE-SIZE "solid" "gray"))
(define b (square SQUARE-SIZE "solid" "blue"))
(define r (square SQUARE-SIZE "solid" "red"))
(define j (square SQUARE-SIZE "solid" "transparent"))

(define SPACESHIP (above
                   (beside j j j j j j j g j j j j j j j)   ;1
                   (beside j j j j j j j g j j j j j j j)   ;2
                   (beside j j j j j j j g j j j j j j j)   ;3
                   (beside j j j j j j g g g j j j j j j)   ;4
                   (beside j j j j j j g g g j j j j j j)   ;5
                   (beside j j j j j j g g g j j j j j j)   ;6
                   (beside j j j r j j g g g j j r j j j)   ;7
                   (beside j j j r j j g g g j j r j j j)   ;8
                   (beside j j j g j g g g g g j g j j j)   ;9
                   (beside r j j g b g g r g g b g j j r)   ;10
                   (beside r j j b g g r r r g g b j j r)   ;11
                   (beside g j j g g g r g r g g g j j g)   ;12
                   (beside g j g g g g g g g g g g g j g)   ;13
                   (beside g g g g g r g g g r g g g g g)   ;14
                   (beside g g g j r r g g g r r j g g g)   ;15
                   (beside g g j j r r j g j r r j j g g)   ;16
                   (beside g j j j j j j g j j j j j j g))) ;17
;                          1 2 3 4 5 6 7 8 9 0 1 2 3 4 5

(define yy (square SQUARE-SIZE "solid" "yellow"))
(define o (square SQUARE-SIZE "solid" "orange"))

(define LASER (overlay/offset
               (above
                yy
                yy)
               40 0
               (above
                yy
                yy)))


(define MTS (empty-scene WIDTH HEIGHT "black"))

;; ====================
;; Data Definitions:

(define-struct spaceship (x y))
;; Spaceship is (make-spaceship Integer Integer)
;; interp. the x, y pos. of the spaceship on MTS
(define S1 (make-spaceship 30 (- HEIGHT 75)))
(define S2 (make-spaceship (/ WIDTH 2) (- HEIGHT 75)))

#;
(define (fn-for-spaceship s)
  (... s))


(define-struct laser (x y))
;; Laser is
;;  - (make-laser (spaceship-x spaceship) (- (spaceship-y spaceship) 18)
;; interp. the x, y pos. of both lasers on MTS based of spaceship x pos.

(define L1 (make-laser (spaceship-x S1)
                       (- (spaceship-y S1) 18)))
(define L2 (make-laser (spaceship-x S2)
                       (- (spaceship-y S2) 18)))
#;
(define (fn-for-laser l)
  (... l))



;; ListOfLaser is one of:
;;  - empty
;;  - (cons Laser ListOfLaser)
(define LOL1 empty)
(define LOL2 (list L1 L2))

#;
(define (fn-for-lol lol)
  (cond [(empty? lol) (...)]
        [else 
         (... (fn-for-laser (first lol))
              (fn-for-lol (rest lol)))]))


(define-struct game (spaceship lol))
;; Game is (make-game Spaceship Laser) ; later on we will add fields to game
;; interp. the game state with the spaceship and lasers
(define G1 (make-game S1 LOL1))
(define G2 (make-game S2 LOL2))

#;
(define (fn-for-game g)
  (... (fn-for-spaceship (game-spaceship g))
       (fn-for-lol (game-lol g))))

;; =================
;; Functions:

;; Game -> Game
;; start the world with (main START-WORLD)

(define (main g)
  (big-bang g                         ; Game
            (on-tick   next-game)     ; Game -> Game
            (to-draw   render-game)   ; Game -> Image
            (on-key    handle-key)))  ; Game KeyEvent -> Game

;; Game -> Game
;; produce next-game
(check-expect (make-game S1 empty)
              (make-game S1 empty))
(check-expect 
 (next-game (make-game
             S1
             (list (make-laser (spaceship-x S1) 99))))
 (make-game
  S1
  (list (make-laser 
         (spaceship-x S1) (- 99 SPEED-LASER)))))

(check-expect
 (next-game (make-game S2
                       (list 
                        (make-laser (spaceship-x S2) 50)
                        (make-laser (spaceship-x S2) 40)
                        (make-laser (spaceship-x S2) 30))))
 (make-game S2
            (list  
             (make-laser (spaceship-x S2) (- 50 SPEED-LASER))
             (make-laser (spaceship-x S2) (- 40 SPEED-LASER))
             (make-laser (spaceship-x S2) (- 30 SPEED-LASER)))))
(check-expect 
 (next-game (make-game S2
                       (list (make-laser 0 5))))
 (make-game S2
            empty))

;(define (next-game g) empty)

(define (next-game g)
  (make-game (game-spaceship g)
             (next-lol (game-lol g))))


;; ListOfLol -> ListOfLol
;; advance lol upwards

(define (next-lol lol)
  (cond [(empty? lol) empty]
        [else
         (if (> (- (- (laser-y (first lol)) SPEED-LASER)
                   (/ (image-height LASER) 2))
                0)
             (cons (next-laser (first lol))
                   (next-lol (rest lol)))
             empty)]))

;; Laser -> Laser
;; move a laser upwards

(define (next-laser l)
  (make-laser (laser-x l)
              (- (laser-y l) SPEED-LASER)))



;; Game -> Image
;; render the current game
(check-expect (render-game (make-game S2 LOL1))
              (render-spaceship S2
                                (render-lol (game-lol (make-game S2 LOL1)))))
(check-expect (render-game (make-game S2 LOL2))
              (render-spaceship S2
                                (render-lol (game-lol (make-game S2 LOL2)))))

;(define (render-game g) empty-image)

(define (render-game g)
  (render-spaceship (game-spaceship g)
                    (render-lol (game-lol g))))


;; Spaceship Image -> Image
;; render the spaceship onto the current MTS
(check-expect (render-spaceship S2 MTS)
              (place-image SPACESHIP (spaceship-x S2) (spaceship-y S2) MTS))



;(define (render-spaceship s mts) empty-image)

(define (render-spaceship s img)
  (place-spaceship s img))


;; Spaceship (ListOfLaser -> Image) -> Image
;; render the spaceship onto the current MTS

;(define (place-spaceship s mts) empty-image)

(define (place-spaceship s lol)
  (place-image SPACESHIP (spaceship-x s) (spaceship-y s) lol))


;; ListOfLaser -> Image
;; place laser onto MTS
(check-expect (render-lol LOL1) MTS)
(check-expect (render-lol (cons (make-laser 100 99)
                                (cons (make-laser 300 200) empty)))
              (place-image LASER
                           100 99
                           (place-image LASER 300 200 MTS)))


(define (render-lol lol)
  (cond [(empty? lol) MTS]
        [else 
         (place-laser (first lol)               
                      (render-lol (rest lol)))]))


;; Laser Image -> Image
;; place laser onto MTS

;(define (place-laser l img) empty-image)

(define (place-laser l img)
  (place-image LASER (laser-x l) (laser-y l) img))



;; Game KeyEvent -> Game
;; move the spaceship from left to right and shoot lasers within the game
;;  don't let the spaceship exit the screen
(check-expect (handle-key (make-game (make-spaceship 20 80) (list L1)) "p")
              (make-game (make-spaceship 20 80) (list L1)))

(check-expect (handle-key (make-game (make-spaceship 99 178) (list L2)) "a")
              (make-game (make-spaceship (- 99 SPEED-SPACESHIP) 178) (list L2)))
(check-expect (handle-key (make-game (make-spaceship 288 300) (list L1)) "d")
              (make-game (make-spaceship (+ 288 SPEED-SPACESHIP) 300)
                         (list L1)))
(check-expect (handle-key (make-game (make-spaceship 100 175) (list L2)) "left")
              (make-game (make-spaceship (- 100 SPEED-SPACESHIP) 175)
                         (list L2)))
(check-expect (handle-key (make-game (make-spaceship 250 150)
                                     (list L1)) "right")
              (make-game (make-spaceship (+ 250 SPEED-SPACESHIP) 150)
                         (list L1)))
(check-expect (handle-key (make-game S1 empty) " ")
              (make-game S1
                         (list (make-laser (spaceship-x S1)
                                           (- (spaceship-y S1) 18)))))
(check-expect (handle-key 
               (make-game
                (make-spaceship
                 (- WIDTH (/ (image-width SPACESHIP) 2))
                 150) L1) "right")
              (make-game (make-spaceship (- WIDTH (/ (image-width SPACESHIP) 2))
                                         150)
                         L1))
(check-expect (handle-key (make-game S1 (list (make-laser 33 99))) " ")
              (make-game S1
                         (list (make-laser (spaceship-x S1)
                                           (- (spaceship-y S1) 18))
                               (make-laser 33 99))))

;(define (handle-key g ke) empty)

(define (handle-key g ke)
  (cond [(or (key=? ke "left") (key=? ke "right") (key=? ke "a") (key=? ke "d"))
         (handle-spaceship (game-spaceship g) (game-lol g) ke)]
        [(key=? ke " ") (handle-laser (game-spaceship g) (game-lol g) ke)]
        [else
         g]))


;; Spaceship Laser KeyEvent -> Spaceship

;(define (handle-spaceship s ke) empty)

(define (handle-spaceship s l ke)
  (cond [(or (key=? ke "a") (key=? ke "left"))
         (if (> (- (spaceship-x s) SPEED-SPACESHIP)
                (+ 0 (/ (image-width SPACESHIP) 2)))
             (make-game (make-spaceship (- (spaceship-x s) SPEED-SPACESHIP)
                                        (spaceship-y s)) l)
             (make-game s l))]
        [(or (key=? ke "d") (key=? ke "right"))
         (if (< (+ (spaceship-x s) SPEED-SPACESHIP)
                (- WIDTH (/ (image-width SPACESHIP) 2)))
             (make-game (make-spaceship (+ (spaceship-x s) SPEED-SPACESHIP)
                                        (spaceship-y s)) l)
             (make-game s l))]
        [else
         (make-game s l)]))


;; Spaceship Laser KeyEvent -> Laser

;(define (handle-laser l ke) empty)

(define (handle-laser s l ke)
  (cond [(key=? ke " ") 
         (if (empty? l)
             (make-game s
                        (list (make-laser (spaceship-x s)
                                          (- (spaceship-y s) 18))))
             (make-game s
                        (append (list (make-laser (spaceship-x s)
                                                  (- (spaceship-y s) 18)))
                                l)))]
        [else
         (make-game s l)]))




(define START-WORLD (make-game 
                     (make-spaceship (/ WIDTH 2) (- HEIGHT 75))
                     empty))