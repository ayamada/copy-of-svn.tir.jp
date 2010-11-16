;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 使い方:
;;; :responderに以下のような手続きをセットし、setp-server:startするだけ。
;;; - 引数として、requestとrequest-headersを取る。
;;; - 返り値として、以下のどちらかを返す。
;;; -- requestに対応するresponse
;;; -- requestに対応するresponseとresponse-headersの二値

;;; ToDo:
;;; :responder内でエラーが発生した場合は、今のところ、
;;; コネクションを維持したまま、エラーヘッダ付きでクライアントに応答を
;;; 自動的に返すが、この仕様は微妙だと思う。
;;; あとでもっとマシな仕様を考えて、直す事。

;;; ToDo: client-disconnect-flagを操作するmethodを提供する事！
;;;       (socket.serverを見る事)
;;; 大体、以下のmethodを用意すれば大丈夫そうだ。
;;; - setp-server:shutdown (済)
;;; - setp-server:client-disconnect
;;; - setp-server:pause

;;; note: :verbose-modeが#tの時は、STDERRに詳細ログを出力する。

;;; ToDo: ロックファイル(pidファイル)を生成する


(define-module tir03.setp.server
  (use gauche.parameter)

  (use srfi-1)
  (use srfi-2) ; and-let*

  ;(use file.util)
  (use util.list)

  (use tir03.socket.server)
  (use tir03.setp.protocol)

  (export
    <setp-server>
    setp-server:start
    setp-server:shutdown

    ;;setp-server:client-disconnect
    ;;setp-server:pause

    ;; came from tir03.socket.server
    <sockaddr>
    <sockaddr-in>
    <sockaddr-un>
    sockaddr-family
    sockaddr-name
    sockaddr-addr
    sockaddr-port
    ))
(select-module tir03.setp.server)


;;; --------
;;; came from tir03.socket.server


(define <sockaddr> <sockaddr>)
(define <sockaddr-in> <sockaddr-in>)
(define <sockaddr-un> <sockaddr-un>)
(define sockaddr-family sockaddr-family)
(define sockaddr-name sockaddr-name)
(define sockaddr-addr sockaddr-addr)
(define sockaddr-port sockaddr-port)


;;; --------


(define-macro (string-append-constant . strings)
  (apply string-append strings))


;;; --------


(define-class <setp-server> ()
  (
   ;; settings
   (responder
     :accessor responder-of
     :init-keyword :responder
     :init-value #f) ; 引数1とoptional引数1を取る手続きを設定する事。
   (sockaddr
     :accessor sockaddr-of
     :init-keyword :sockaddr
     :init-value #f)
   (verbose-mode
     :accessor verbose-mode-of
     :init-keyword :verbose-mode
     :init-value #t)

   ;; internal slots
   (socket-server
     :accessor socket-server-of
     :init-value #f)
   ))


(define-method initialize ((self <setp-server>) initargs)
  (next-method)
  ;; check slot
  (unless (responder-of self)
    (errorf "~a must be need to :responder" (class-name (class-of self))))
  (unless (sockaddr-of self)
    (errorf "~a must be need to :sockaddr" (class-name (class-of self))))
  ;; make :socket-server
  (set!
    (socket-server-of self)
    (make
      <socket-server>
      :sockaddr (sockaddr-of self)
      :verbose-mode (verbose-mode-of self)
      :client-handler (lambda (ccid sockaddr reader writer)
                        (socket-server-client-handler self
                                                      ccid
                                                      sockaddr
                                                      reader
                                                      writer))
      ))
  )



;;; parameterize後は、'connectまたは'disconnectのどちらかとなる
(define client-disconnect-flag (make-parameter #f))




(define (make-error-response template . format-args)
  `(#f
    (("error" ,(if (null? format-args)
                 template
                 (apply format template format-args))))))


(define (socket-server-client-handler self ccid sockaddr reader writer)
  (define (initialize!)
    (setp-logging ccid 'system "client accepted")
    ;; 接続確立時の初期化処理を行う
    #t)
  (define (finalize!)
    (setp-logging ccid 'system "client finished")
    ;; 接続終了時のterminate処理を行う
    #t)
  (define (line->requests line)
    ;; lineをパーズして元のリクエストをlistとして返す
    ;; リクエストが不正だった場合は、<condition>を返す
    (guard (e (else e))
      (call-with-input-string
        line
        (lambda (p)
          (let next ()
            (let1 r (read p)
              (if (eof-object? r)
                '()
                (cons r (next)))))))))
  (define (requests->response requests)
    ;; note: ここに来た段階で、requestsは'()ではない事は保証されている(↓)
    ;; 二値を持つlistを返す。
    ;; - carは、要求されたresponseそのもの。
    ;; - cadrは、rfc.822形式のalist(メタデータを送る際に使う)
    (let ((request (car requests))
          (request-headers (if (= 1 (length requests))
                             '()
                             (cadr requests))))
      (receive response+alist ((responder-of self) request request-headers)
        (define (get-alist)
          ;; ToDo: あとでちゃんと生成するようにする事
          (if (= 1 (length response+alist))
                 '()
                 (cadr response+alist)))
        (let ((response (car response+alist))
              (alist (get-alist)))
          (list response alist)))))

  (parameterize ((client-disconnect-flag 'connect))
    ;; まず最初に、SETPプロトコルとそのバージョンを確認する(handshake)
    ;;;; ToDo: まだ未実装
    (initialize!)
    (let/cc quit&disconnect
      (define (respond-to-client responses)
        (with-error-handler
          (lambda (e)
            ;; ここに来る = write失敗 = 通信不能、なので、disconnectする。
            ;; ToDo: conditionを見るようにする
            (setp-logging ccid 'respond-error (ref e 'message))
            (quit&disconnect))
          (lambda ()
            (if (null? responses)
              #t ; responsesが'()なら、空行を返す？それともエラーを返す？
              (let next ((current (car responses))
                         (left (cdr responses)))
                (writer 'write/ss current)
                (unless (null? left)
                  (writer 'display " ")
                  (next (car left) (cdr left)))))
            (writer 'newline)
            (writer 'flush))))

      ;; requestを受け取り、一行ずつ処理していく
      (let next-line ()
        (when (eq? 'disconnect (client-disconnect-flag))
          (quit&disconnect))
        ;; 一行を取得
        (let1 line (reader 'read-line #t) ; バイナリデータの可能性有り？
          (when (eof-object? line)
            (setp-logging ccid 'system "caught #<eof>")
            (quit&disconnect))
          (let1 requests (line->requests line)
            (cond
              ((condition? requests)
               ;; 不正リクエストだった
               ;; 接続は維持したまま、エラーを返す
               ;; ToDo: 切断した方が良いかどうか考える
               (setp-logging ccid 'invalid-request-line line)
               (respond-to-client
                 (make-error-response (ref requests 'message))))
              (else
                (setp-logging ccid 'request-accepted requests)
                (guard (e (else
                            (setp-logging ccid
                                          'internal-error
                                          (ref e 'message))
                            (respond-to-client
                              (make-error-response (ref e 'message)))))
                  (respond-to-client (requests->response requests)))))))
        (next-line)))
    (finalize!)))



;;; --------


(define (setp-logging ccid log-type loggee)
  ;; ToDo: 引数の変更
  ;; ToDo: 仕様の見直しと出力の実装
  #f)



(define (setp-server:open self)
  (setp-logging #f 'system "booted")
  ;; (必要なら)初期化処理を行う
  #t)




(define (setp-server:close self)
  (setp-logging #f 'system "shutdowned")
  ;; (必要なら)terminate処理を行う
  #t)



(define-method setp-server:start ((self <setp-server>))
  ;; ToDo: pidファイルを使って、ロックするようにする
  (dynamic-wind
    (cut setp-server:open self)
    (cut socket-server:start (socket-server-of self))
    (cut setp-server:close self)))


(define-method setp-server:shutdown ((self <setp-server>))
  (socket-server:shutdown (socket-server-of self)))

(provide "tir03/setp/server")


