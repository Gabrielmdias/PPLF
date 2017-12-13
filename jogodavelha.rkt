#lang racket/gui

(require 2htdp/image 2htdp/universe)
(require (prefix-in htdp: 2htdp/image))
(require lang/posn)


; Make a frame by instantiating the frame% class
(define frame1 (new frame% [label "Jogo da Velha"]
                   [min-width 512]
                   [min-height 512]))
 
; Make a static text message in the frame
(define player (new text-field% [parent frame1]
                          [label "Jogador 1 'X' :"]
                          [min-width 80]
                          [stretchable-width #f]))

(define player2 (new text-field% [parent frame1]
                          [label "Jogador 2 'O' :"]
                          [min-width 80]
                          [stretchable-width #f]))

(define playerName "player")
; Make a button in the frame
(new button% [parent frame1]
             [label "Start"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (set! playerName (send player get-value))
						 (set! playerName (send player2 get-value))
                         (send frame show #t)
                         (send frame1 show #f)
                         )])
 
; Show the frame by calling its show method
(send frame1 show #t)

;(define configInicial (make-vector 9 "*"))

(define configInicial (vector "*" "*" "*" "*" "*" "*" "*" "*" "*"))


;(struct jogador (simbulo valor))
;(define jogador1 (jogador "x" 1))
;(define jogador2 (jogador "o" -1))

(define controle 1)


(define quadrado (square 100 "outline" "black"))
(define tela (empty-scene 300 300))

(define (converteEntrada valor)
	(cond
	[(equal? valor "x") (text valor 50 "red")]
	[(equal? valor "o") (text valor 50 "blue")]
	[else (text " " 50 "white")]
	))


(define (clicar-mouse w x y event)
	(cond
		[(mouse=? event "button-down") (posicao x y)]
		[else w]))

 (define (posicao x y)
 	(cond
	 	[(and (< x 100) (< y 100)) (vector-set! configInicial 0 (vezJogador 0))]
		[(and (< x 200) (< y 100)) (vector-set! configInicial 1 (vezJogador 1))]
		[(and (< x 300) (< y 100)) (vector-set! configInicial 2 (vezJogador 2))]
		[(and (< x 100) (< y 200)) (vector-set! configInicial 3 (vezJogador 3))]
		[(and (< x 200) (< y 200)) (vector-set! configInicial 4 (vezJogador 4))]
		[(and (< x 300) (< y 200)) (vector-set! configInicial 5 (vezJogador 5))]
		[(and (< x 100) (< y 300)) (vector-set! configInicial 6 (vezJogador 6))]
		[(and (< x 200) (< y 300)) (vector-set! configInicial 7 (vezJogador 7))]
		[(and (< x 300) (< y 300)) (vector-set! configInicial 8 (vezJogador 8))]))

(define (vazio pos)
	(cond
		[(equal? (vector-ref configInicial pos) "*") #t]
		[else #f]))

(define (arruma pos)
	(set! controle (* controle -1))
	(cond
		[(equal? (vector-ref configInicial pos) "x") "x"]
		[(equal? (vector-ref configInicial pos) "o") "o"]))

(define (vezJogador pos)
	(set! controle (* controle -1))
	(cond 
		[(and (< controle 0) (vazio pos)) "x"]
		[(and (> controle 0) (vazio pos)) "o"]
		[else (arruma pos)]))


(define (verifica posicao)
	(cond
		[(equal? (vector-ref configInicial posicao) "x") 1]
		[(equal? (vector-ref configInicial posicao) "o") -1]
		[(equal? (vector-ref configInicial posicao) "*") 0]))

(define vencedor
	(cond
		[(and(and (equal? (verifica 0) 1) (equal? (verifica 1) 1)) (equal? (verifica 2) 1)) "Jogador 1 ganhou"]
		[(and(and (equal? (verifica 3) 1) (equal? (verifica 4) 1)) (equal? (verifica 5) 1)) "Jogador 1 ganhou"]
		[(and(and (equal? (verifica 6) 1) (equal? (verifica 7) 1)) (equal? (verifica 8) 1)) "Jogador 1 ganhou"]
		[(and(and (equal? (verifica 0) 1) (equal? (verifica 3) 1)) (equal? (verifica 6) 1)) "Jogador 1 ganhou"]
		[(and(and (equal? (verifica 1) 1) (equal? (verifica 4) 1)) (equal? (verifica 7) 1)) "Jogador 1 ganhou"]
		[(and(and (equal? (verifica 2) 1) (equal? (verifica 5) 1)) (equal? (verifica 8) 1)) "Jogador 1 ganhou"]
		[(and(and (equal? (verifica 0) 1) (equal? (verifica 5) 1)) (equal? (verifica 8) 1)) "Jogador 1 ganhou"]
		[(and(and (equal? (verifica 2) 1) (equal? (verifica 5) 1)) (equal? (verifica 7) 1)) "Jogador 1 ganhou"]
		[(and(and (equal? (verifica 0) -1) (equal? (verifica 1) -1)) (equal? (verifica 2) -1)) "Jogador 2 ganhou"]
		[(and(and (equal? (verifica 3) -1) (equal? (verifica 4) -1)) (equal? (verifica 5) -1)) "Jogador 2 ganhou"]
		[(and(and (equal? (verifica 6) -1) (equal? (verifica 7) -1)) (equal? (verifica 8) -1)) "Jogador 2 ganhou"]
		[(and(and (equal? (verifica 0) -1) (equal? (verifica 3) -1)) (equal? (verifica 6) -1)) "Jogador 2 ganhou"]
		[(and(and (equal? (verifica 1) -1) (equal? (verifica 4) -1)) (equal? (verifica 7) -1)) "Jogador 2 ganhou"]
		[(and(and (equal? (verifica 2) -1) (equal? (verifica 5) -1)) (equal? (verifica 8) -1)) "Jogador 2 ganhou"]
		[(and(and (equal? (verifica 0) -1) (equal? (verifica 5) -1)) (equal? (verifica 8) -1)) "Jogador 2 ganhou"]
		[(and(and (equal? (verifica 2) -1) (equal? (verifica 5) -1)) (equal? (verifica 7) -1)) "Jogador 2 ganhou"]
		[else "tnc"]))

configInicial
vencedor

(define listaQuadrados (list quadrado quadrado quadrado quadrado quadrado quadrado quadrado quadrado quadrado))
(define listaPosicoes (list (make-posn 50 50) (make-posn 150 50) (make-posn 250 50)
							(make-posn 50 150) (make-posn 150 150) (make-posn 250 150)
							(make-posn 50 250) (make-posn 150 250) (make-posn 250 250)))


(define (jogo word)
	(let
	[(tabuleiro (build-list 9 (lambda (x) (converteEntrada (vector-ref configInicial x)))))]
	(place-images (append listaQuadrados tabuleiro) (append listaPosicoes listaPosicoes) tela)))


(big-bang 0
	(name "JOGO DA VELHA")
	(on-mouse clicar-mouse)
	(to-draw jogo))