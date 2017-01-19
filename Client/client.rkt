#lang racket/gui

;;use ``raco pkg install rfc6455`` to install the dependency

(require json)

;; view of chatroom

(define (xgap p size)
  (new pane% [parent p]
       [min-width size]
       [stretchable-width #f]))

(define (ygap p size)
  (new pane% [parent p]
       [min-height size]
       [stretchable-height #f]))

(define chatroom-box%
  (class object% (super-new)
    
    (field [on-submit #f]
           [ myusername "PureMan" ]
           [ mysendlock #f ]
           [ myframe #f]
           [ mytext1 #f]
           [ myinput #f])

    (define/public (submit button event)
      (when (eq? (send event get-event-type) 'text-field-enter)
        (unless mysendlock
          (set! mysendlock #t)
          (send (send myinput get-editor) lock #t)
          (on-submit (send myinput get-value))
          (set! mysendlock #f)
          (send (send myinput get-editor) lock #f)
          (send myinput set-value ""))))
    
    (define/public (frame-make)
      (let* ([myframe (new frame% [label "PureChat 0.01"])]
         
             ;; big grid
             [p1 (new horizontal-panel% [parent myframe]
                      [alignment `(left top)])]
         
             [g1 (xgap p1 20)]
             [p11 (new vertical-panel% [parent p1]
                       [alignment `(left top)])]
             [g2 (xgap p1 20)]
             [p12 (new vertical-panel% [parent p1]
                       [alignment `(left top)])]
             [g3 (xgap p1 20)]

             ;; left side
             [text1 (new text%)]
             [g4 (ygap p11 20)]
             [textarea (new editor-canvas%  [parent p11]
                            [min-width 600]
                            [min-height 400]
                            [style `(auto-hscroll)])]
             [g5 (ygap p11 10)]

             [input-done (lambda (button event)
                           (send this submit button event))]
             
             [input (new text-field% [parent p11]
                         [callback input-done]
                         [label (string-append myusername " :")])]
             [g6 (ygap p11 15)]

             ;; right side
             [g7 (ygap p12 10)]
             [onlines (new list-box% [parent p12]
                           [label "Onlines"]
                           [choices `("abc" "dde" "fff")]
                           [style `(single vertical-label)]
                           [min-width 150]
                           [stretchable-width #f])]
            
             [g8 (ygap p12 10)])
        (set! myframe myframe)
        (set! mytext1 text1)
        (set! myinput input)
        (send textarea set-editor text1)
        (send text1 lock #t)
        (send myframe show #t)))

    (define/public (add-message-line line)
      (let* ([line1 (string-append line "")]
             [wrapper (make-object string-snip% line1)])
        
        (send mytext1 lock #f)
        (send mytext1 insert wrapper (send mytext1 last-position))
        (send mytext1 lock #f)
        (send mytext1 insert "\n" (send mytext1 last-position))
        (send mytext1 lock #t)))

    (define/public (set-onsubmit! onsubmit)
      (set! on-submit onsubmit))

    (define/public (add-message-imageurl prefix image-url)
      (let* ([wrapper (make-object string-snip% (string-join (list prefix ": " image-url) ""))])
        (send mytext1 lock #f)
        (send mytext1 insert wrapper (send mytext1 last-position))
        (send mytext1 lock #f)
        (send mytext1 insert "\n" (send mytext1 last-position))
        (send mytext1 lock #t)
        (thread (lambda ()
                  (let* ([img (make-object image-snip%
                                (read-bitmap (get-pure-port (string->url image-url))))]
                         [pos (+ (send mytext1 get-snip-position wrapper) 1)])
                    (send mytext1 lock #f)
                    (send mytext1 insert "\n" pos)
                    (send mytext1 insert img pos)
                    (send mytext1 insert "  " pos)
                    (send mytext1 lock #t))))))

    (define/public (user-leave who)
      (display "::: LEAVE ::: ")
      (displayln who))


    (define/public (user-join who)
      (display "::: JOIN ::: ")
      (displayln who))

    (define/public (show b)
      (send myframe show b))

    (define/public (set-username username)
      (when myinput
        (send myinput set-label (string-append username " :")))
      (set! myusername username))
    ))

;; view of login box

(define username #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require net/rfc6455)
(require net/url)
(define c (ws-connect (string->url "ws://sapphiresoft.io:8888/")))

(define (chatroom-reader-bind c username callback)
  (ws-send! c (string-append username))
  (thread (lambda ()
            (let loop ()
              (callback (ws-recv c))
              (loop)))))

(define (purechat-handler1 x username message)
  (let* ([sp (string-split message ": " #:trim? #f)]
                                            [spl (length sp)])
                                       (cond
                                         [(<= spl 1)
                                          (send x add-message-line message)]
                                         [ else
                                           (let* ([header (car sp)]
                                                  [hsp (string-split header "@")]
                                                  [content (string-join (cdr sp) ": ")])
                                             (cond
                                               [(and (string-prefix? content "http")
                                                     (or (string-contains? content ".jpg")
                                                         (string-contains? content ".png")
                                                         (string-contains? content ".gif")))
                                                (let * ([who (car hsp)]
                                                        [time (car (cdr hsp))])
                                                  (send x add-message-imageurl header content))]
                                               [ else
                                                 (send x add-message-line message)]))])))

(define (purechat-handler2 x username message)
  (displayln message))


(define (dispatch x username data)
  (let* ([tag (hash-ref data `tag)])
    (cond
        [(equal? tag "TEXT") (send x add-message-line (hash-ref data `contents))]
        [(equal? tag "PIC") (send x add-message-imageurl (hash-ref data `contents))]
        [(equal? tag "JOIN") (send x user-join (hash-ref data `contents))]
        [(equal? tag "LEAVE") (send x user-leave (hash-ref data `contents))]
        [(equal? tag "LIST") (for-each (lambda (m)
                                  (dispatch x username m))
                                (hash-ref data `contents))])))

(define (purechat-handler3 x username message)
  (displayln message)
  (unless (string? message)
    (displayln "EOF")
    (sleep 1))
  (when (and (string? message)
             (> (string-length message) 0))
    (dispatch x username (string->jsexpr message))))

(define (chatroom-go)
  (define x (new chatroom-box%))
  (send x set-username username)
  (send x frame-make)
  (send x set-onsubmit! (lambda (input)                               
                          (ws-send! c input)))
  (chatroom-reader-bind c username (lambda (message)
                                     (purechat-handler3 x username message))))

;;;;;;;;;;;;;;;;;;;;

(define dialog (instantiate dialog% ("Input you name - PureChat")))

(define name-inputer (new text-field% [parent dialog] [label "You name"]))

(define panel (new horizontal-panel% [parent dialog]
                                     [alignment '(center center)]))
 
; Add Cancel and Ok buttons to the horizontal panel
(new button% [parent panel] [label "Cancel"]
     [callback (lambda (button event)
                 (exit 0))])

(new button% [parent panel] [label "Start Chat"]
     [callback (lambda (button event)
                 (set! username (send name-inputer get-value))
                 (display username)
                 (display "\n")
                 (send dialog show #f)
                 (chatroom-go))])  ;;ugly
 
; Show the dialog
(send dialog show #t)

;(set! username "guest")
;(chatroom-go)