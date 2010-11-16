#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme sw=2 ts=2 et:
;;; $Id$

;;; tcpcgiの実行サンプル。

;;; usage :
;;; cd /path/to/here
;;; env - PATH="$PATH" \
;;; tcpserver -v -c 16 -h -R -u xxxx -g yyyy -x ./tcpcgi.cdb 0 80 \
;;; gosh ./tcpcgi-kickstart.scm
;;; # この時のtcpserverへのパラメータはお好みで。

;;; 動作確認をするだけなら、以下を実行してhttp://localhost:8888 にアクセス。
;;; env - PATH="$PATH" tcpserver -v -c 8 -h -R 0 8888 tcpcgi-kickstart.scm



(add-load-path "lib")
(use tcpcgi)

(use gauche.process)
(use srfi-1)
(use text.tree)
(use text.html-lite)
(use www.cgi)
(use wiliki)


;;; ----------------------------------------------------------------
;;; ここから下は、サンプルcgi定義

(define (debug-cgi)
  (cgi-main
    (lambda (params)
      (list
        (cgi-header :content-type "text/html")
        (html:html
          (html:body
            (html:h1 "cgi-metavariables")
            (html:dl
              (map (lambda (x)
                     (list
                       (html:dt (html-escape-string (car x)))
                       (html:dd (html-escape-string (cadr x)))))
                   (sort
                     (cgi-metavariables)
                     (lambda (x y)
                       (string<? (car x) (car y))))))
            (html:hr)
            (html:h1 "form-parameters")
            (html:dl
              (map (lambda (x)
                     (list
                       (html:dt (html-escape-string (car x)))
                       (map
                         (lambda (xx)
                           (html:dd (html-escape-string xx)))
                         (cdr x))))
                   params))
            (html:hr)
            (html:h1 "environment")
            (html:ul
              (map
                (lambda (x)
                  (html:li (html-escape-string x)))
                (process-output->string-list "/usr/bin/env")))
            ))))
    :on-error report-error
    ))


(define delayed-wiliki
  (delay (make <wiliki>
           :db-path "/tmp/wiliki-test"
           :log-file "wiliki-test.log"
           :top-page "wiliki-test"
           :title "wiliki-test"
           :language 'jp
           :charsets '((jp . euc-jp) (en . euc-jp))
           :debug-level 1
           :editable? #t
           )))

(define (wiliki-cgi)
  (wiliki-main (force delayed-wiliki)))


(define (menu-cgi)
  (write-tree
    (list
      (cgi-header)
      (html:html
        (html:body
          (html:h1 "tcpcgi sample")
          (html:ul
            (map
              (lambda (l)
                (html:li
                  (html:a
                    :href (car l)
                    (car l))))
              *path-dispatch*)))))))

(define (status-cgi)
  (write-tree
    (list
      (cgi-header
        :status "123 Hoge"
        )
      (html:html
        (html:body
          (html:p "Status: 123 Hoge"))))))

(define (hello-cgi)
  (write-tree
    (list
      (cgi-header)
      (html:html
        (html:body
          (html:p "hello"))))))

(define (sleep60-cgi)
  (for-each
    (lambda (x)
      (sys-sleep 1))
    (iota 60))
  (hello-cgi))

(define (script-error-cgi)
  (write-tree
    (list
      (cgi-header)
      (html:html
        (html:body
          (html:p "hoge-error start")
          (error "hoge-error-occured")
          (html:p "hoge-error end"))))))

(define (no-contents-cgi)
  #f)

(define (invalid-header-cgi)
  (print "hoge-invalid-string"))

(define (location1-cgi)
  (cgi-main
    (lambda (params)
      (list
        (cgi-header
          :location "http://google.com/"
          )))))
(define (location2-cgi)
  (cgi-main
    (lambda (params)
      (list
        (cgi-header
          :location "/env/aaa/bbb/ccc?ddd=eee&fff=ggg"
          )))))
(define (location3-cgi)
  (cgi-main
    (lambda (params)
      (list
        (cgi-header
          :location "hoge.cgi"
          )))))

(define (nph-cgi)
  (write-tree
    (list
      "HTTP/1.0 200 OK\r\n"
      "Content-Type: text/html\r\n"
      "Pragma: no-cache\r\n"
      "\r\n"
      (html:html
        (html:head
          (html:title "test of nph-script"))
        (html:body
          (html:h1 "this is test of nph-script.")
          (html:p "これはnphスクリプトの動作テストです。")
          (html:hr)
          (html:p
            (map
              (lambda (x)
                "ふがふが")
              (iota 32)))
          (html:hr)
          (html:p "カウントします。")
          ))))
  (for-each
    (lambda (num)
      (flush)
      (sys-sleep 1)
      (display (x->string num))
      (print "<br />")
      (flush))
    (iota 4)))

(define (error404-cgi)
  (write-tree
    (list
      (cgi-header
        :status "404 Not Found"
        :pragma "no-cache"
        )
      (html:html
        (html:head
          (html:title "404 のっと ふぁうんど")
          )
        (html:body
          (html:h1 "のっと ふぁうんど")
          (html:p "みつかりません"))))))

;;; ここから上は、サンプルcgi定義
;;; ----------------------------------------------------------------
;;; ここから下は、tcpcgi設定部分



;; alistで指定する。
(define *path-dispatch*
  `(
    ;; cgi動作確認用
    ("/" ,menu-cgi) ; これのみ、/にのみマッチする
    ("/wiliki" ,wiliki-cgi)
    ("/env" ,debug-cgi)
    ("/hello" ,hello-cgi)
    ("/status" ,status-cgi)
    ("/nph-script" ,nph-cgi :nph #t) ; nph動作
    ("/location1" ,location1-cgi) ; full uri指定
    ("/location2" ,location2-cgi) ; 絶対path指定
    ("/location3" ,location3-cgi) ; 相対path指定(bad)
    ("/sleep60" ,sleep60-cgi)

    ;; ファイル表示動作確認用
    ("/cpuinfo" "/proc/cpuinfo") ; ファイル表示
    ("/qmail-doc" "/var/qmail/doc") ; ディレクトリ表示(FAQとかINSTALLが見れる）
    ("/robots.txt" "/path/to/robots.txt") ; ファイル表示(404)
    ("/favicon.ico" #f) ; 明示的に404を返す

    ;; 以下は、cgiエラー時の動作確認用
    ("/no-contents" ,no-contents-cgi)
    ("/script-error" ,script-error-cgi)
    ("/invalid-header" ,invalid-header-cgi)
    ))




(define *tcpcgi*
  (make <tcpcgi>
    ;; 以下の:*-dispatchは、この順に処理される。
    ;:vhost-dispatch `(("hoge.com" ,*path-dispatch*) ; hoge.com
    ;                  (".hoge.com" ,*path-dispatch*) ; *.hoge.com
    ;                  )
    ;; vhostのどれにもマッチしなかった場合はpath-dispatchが実行される。
    :path-dispatch *path-dispatch*
    ;; pathのどれにもマッチしなかった場合はnone-dispatchが実行される。
    ;; none-dispatchも無指定なら、404が返される。
    ;:none-dispatch debug-cgi
    ;:none-dispatch (list nph-cgi :nph #t) ; nph等にしたい場合はこうする

    ;; このエラードキュメント設定のインターフェースは将来変更されます。
    :errordoc-table (hash-table
                      'eqv?
                      `(404 . ,error404-cgi))

    ;; タイムアウト値等の設定
    :request-timeout 30 ; クライアントからのHTTPヘッダ読み出し時のタイムアウト
    ;; ↑人間が手入力をするなら、大きな値にする。通常は5ぐらい。
    :response-timeout 60 ; cgi実行時のタイムアウト
    :keepalive-timeout 20 ; keep-aliveタイムアウト
    ;; ↑reverse-proxyを使うなら、大きな値にする。
    ;; ↑人間が手入力をするなら、大きな値にする。通常は5ぐらい。
    :use-server-header #t ; Serverヘッダを送るか否か。デフォルトは#f。
    ))

;;; ----

;; tcpserverを使うなら、環境変数から必要なパラメータを取得できる。
(define (main args)
  (tcpcgi-main
    *tcpcgi*
    (sys-getenv "TCPLOCALIP") ; SERVER_ADDR (必須)
    (sys-getenv "TCPLOCALPORT") ; SERVER_PORT (必須)
    (sys-getenv "TCPLOCALHOST") ; SERVER_NAME (必須)※
    (sys-getenv "TCPREMOTEIP") ; REMOTE_ADDR (必須)
    (sys-getenv "TCPREMOTEPORT") ; REMOTE_PORT (必須)
    (sys-getenv "TCPREMOTEHOST") ; REMOTE_HOST or #f
    #f ; HTTPS flag
    ))
;; ※SERVER_NAMEは、:vhost-dispatchにマッチした場合に上書きされるが、
;; それ以外の場合はそのままこの値が使用される。
;; このSERVER_NAMEは、Locationヘッダに完全でないuriが渡された時に
;; 自動補完されるサーバ名としても使われる。



