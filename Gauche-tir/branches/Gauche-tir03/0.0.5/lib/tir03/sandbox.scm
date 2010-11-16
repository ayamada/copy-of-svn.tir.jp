;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; sandbox module

;;; このモジュールは、不特定多数からの接続に対して、
;;; それぞれ独立した無名モジュール空間を提供する為の、
;;; sandboxモジュールを生成する為の関数類を提供するモジュールです。
;;; - まず、一旦、sandboxの元となる、テンプレートモジュール(名前有り)を、
;;;   make-template-moduleによって、生成します。
;;; - そして、その後、make-sandbox-module-from-template-moduleによって、
;;;   テンプレートモジュールから、無名モジュールを生成します。

;;; note: このモジュールは、(悪意を持っているかも知れない)不特定多数に
;;;       S式を評価させる為の、絶対安全なsandboxを生成するものではありません。
;;;       現在のところ、nullベースのsandboxであっても、容易にDoSを
;;;       起こす事が可能です。
;;;       あくまでも、競合状態を避ける為のものである事に注意して下さい。

;;; note: 現在のところ、gaucheベースのsandboxでは、あらゆる機能が
;;;       有効になっています。
;;;       これは、モジュールのrequire/provide時や、クラスの定義等に、
;;;       競合状態が発生したりする可能性があるという事です。
;;;       そのような問題を避けたい場合は、scheme(またはr5rs)ベースのsandboxを
;;;       生成するようにして下さい。
;;;       或いは、そのような状態にならないように、注意深く使用するように
;;;       して下さい。

;;; note: schemeベースのsandboxに制御を渡す際に、stdin/stdout/errorの各portは、
;;;       変更しておいた方がより安全です。
;;;       (これらまでは、このモジュールでは面倒を見る事が出来ません)


(define-module tir03.sandbox
  (export
    make-template-module
    make-sandbox-module-from-template-module
    ))
(select-module tir03.sandbox)


;;; --------


(define-syntax disable-bindings
  (syntax-rules ()
    ((_ module) #f)
    ((_ module name . names)
     (begin
       (eval `(define (name . args)
                (,errorf "~a can't be used within this module"
                         'name))
             module)
       (disable-bindings module . names)))))


(define (disable-unsafe-r5rs-bindings module)
  (disable-bindings module
    open-input-file open-output-file
    call-with-input-file call-with-output-file
    with-input-from-file with-output-to-file
    load
    null-environment scheme-report-environment 
    interaction-environment)
  module)


;;; --------


(define (make-template-module module-name-symbol . opt-type)
  (let ((type (get-optional opt-type 'gauche))
        (module (make-module module-name-symbol)))
    (case type
      ((gauche)
       ;; この時は、特に禁則処理は入れない。
       (eval '(extend gauche)))
      ((scheme r5rs)
       (eval '(extend scheme))
       ;; 安全でない可能性のある束縛を禁止する。
       (disable-unsafe-r5rs-bindings module))
      ((null)
       ;; 禁則処理は不要な筈……。
       (eval '(extend null)))
      (else
        (error "invalid type" type)))
    ;; ToDo: 他に必要な処理は？
    module))


(define (make-sandbox-module-from-template-module template-name)
  (let1 template-module (find-module template-name)
    (let1 sandbox-module (make-module #f)
      ;; sandbox-moduleの親をtemplate-moduleにする
      (eval `(extend ,template-name) sandbox-module)
      (when (global-variable-bound? sandbox-module 'interaction-environment)
        ;; interaction-environmentは、自分自身とする
        (eval `(define (interaction-environment) ,sandbox-module)
              sandbox-module))
      sandbox-module)))


;;; --------


(provide "tir03/sandbox")


