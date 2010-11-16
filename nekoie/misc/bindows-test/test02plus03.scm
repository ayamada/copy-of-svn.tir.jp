;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$



(define script-filename "test02plus03.js")

(define root-window-caption "bindows test")
(define root-window-width 512)
(define root-window-height 128)

(define margin 4)
(define input-field-width 48)
(define button-width 16)
(define item-height 24)

(define (elem-ref elem-list key-symbol . opt-fallback)
  (let1 result-list (assq key-symbol elem-list)
    (cond
      (result-list (cadr result-list)) ; ToDo: support for invalid elem-list
      ((null? opt-fallback) (errorf "not found ~s in ~s" key-symbol elem-list))
      (else
        (car opt-fallback)))))


(define if1-elem `((id "inputField1")
                   (left ,margin)
                   (top ,margin)
                   (width ,input-field-width)
                   (height ,item-height)
                   (align "right")
                   ))
(define ob-elem `((id "operandButton")
                  (left ,(+ (elem-ref if1-elem 'left)
                            (elem-ref if1-elem 'width)
                            margin))
                  (top ,margin)
                  (width ,button-width)
                  (height ,item-height)
                  (align "center")
                  ))
(define if2-elem `((id "inputField2")
                   (left ,(+ (elem-ref ob-elem 'left)
                             (elem-ref ob-elem 'width)
                             margin))
                   (top ,margin)
                   (width ,input-field-width)
                   (height ,item-height)
                   (align "right")
                   ))
(define rb-elem `((id "resultButton")
                  (left ,(+ (elem-ref if2-elem 'left)
                            (elem-ref if2-elem 'width)
                            margin))
                  (top ,margin)
                  (width ,button-width)
                  (height ,item-height)
                  (align "center")
                  (command "#send-command")
                  ))
(define rf-elem `((id "resultField")
                  (left ,(+ (elem-ref rb-elem 'left)
                            (elem-ref rb-elem 'width)
                            margin))
                  (right ,margin)
                  (top ,margin)
                  (height ,item-height)
                  (align "right")
                  (readOnly "true")
                  ))


;;; sxml-tree
`(Application
   (Window
     (@ (caption ,root-window-caption)
        (width ,root-window-width)
        (height ,root-window-height)
        )
     (Command (@ (id "send-command") (shortcut "Alt+S")))
     (TextField (@ ,@if1-elem) "10")
     (Button (@ ,@ob-elem) "/")
     (TextField (@ ,@if2-elem) "3")
     (Button (@ ,@rb-elem) "=")
     (TextField (@ ,@rf-elem))
     (StatusBar
       (@ (right "0")
          (left "0")
          (bottom "0")
          )
       (StatusBarPanel
         (@ (right "100")
            (left "0")
            (id "status")
            )
         )
       (StatusBarPanel
         (@ (width "98")
            (right "0")
            )
         (UndeterminedProgressBar
           (@ (visible "false")
              (top "0")
              (right "0")
              (left "0")
              (id "pb")
              (bottom "0")
              (border "0")
              (backColor "transparent")
              )))))
   ;(String (@ (id "xsm-url")) "http://www.tir.ne.jp/~nekoie/b/xsm.cgi")
   (Resources
     (Script
       (@ (src ,script-filename)))))


