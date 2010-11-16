;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 以下のurlのコードをベースにした、カレンダーモジュール。
;;; http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi?Gauche%3aCGI%3a%a5%b9%a5%b1%a5%b8%a5%e5%a1%bc%a5%eb%cd%bd%c4%ea%c9%bd%3aShiro%c8%c7

;; NB: date型とtime型は、基本的に同じものを指している筈なのに、
;;     行える演算が違ったりして不便。
;;     このままでは矢鱈と変換する必要が出てしまう。
;;     そこで、両方の型を透過的に扱えるdatetime型を用意した。
;;     但し、今は適当に作ったのでインターフェースが非常に適当。
;;     将来的には、date型のmethodもtime型のmethodも直接呼べるようにすべき。




(define-module tir04.util.calendar
  (use srfi-19)
  (use srfi-1)
  (use util.list)

  (export
    <datetime>
    date->datetime
    time->datetime
    datetime->date
    datetime->time
    update-datetime!
    year&month&day->datetime
    yyyymmdd->datetime
    datetime->calendar-list
    prev-month-datetime
    next-month-datetime
    ))
(select-module tir04.util.calendar)


(define *current-date-zone-offset*
  (date-zone-offset (current-date)))

(define *one-day-duration*
  (make-time time-duration 0 (* 24 60 60)))


(define-class <datetime> ()
  (
   (date
     :accessor date-of
     :init-keyword :date
     :init-value #f)
   (time
     :accessor time-of
     :init-keyword :time
     :init-value #f)
   ))

(define (date->datetime src-date)
  (make <datetime> :date src-date))

(define (time->datetime src-time)
  (make <datetime> :time src-time))

(define-method datetime->date ((self <datetime>))
  (or
    (date-of self)
    (let1 date (time-utc->date (time-of self) *current-date-zone-offset*)
      (set! (date-of self) date)
      date)))

(define-method datetime->time ((self <datetime>))
  (or
    (time-of self)
    (let1 time (date->time-utc (date-of self))
      (set! (time-of self) time)
      time)))

(define-method update-datetime! ((self <datetime>) proc)
  ;; TODO: あとで
  (error "sorry, not implemented"))



(define (year&month&day->datetime . args)
  (if (null? args)
    (time->datetime (current-time))
    (let-optionals* args ((year 1)
                          (month 1)
                          (day 1))
      (date->datetime
        (make-date 0 0 0 0 day month year *current-date-zone-offset*)))))


(define (yyyymmdd->datetime yyyymmdd)
  (or
    (and-let* ((m (#/^(\d\d\d\d)(\d\d)?(\d\d)?$/ (x->string yyyymm)))
               (num (rxmatch-num-matches m))
               (yyyy (x->integer (m 1)))
               (mm (if (< 1 num)
                     (x->integer (m 2))
                     1))
               (dd (if (< 2 num)
                     (x->integer (m 3))
                     1))
               )
      (and
        ;; yyyyは今のところチェック無し
        (<= 1 mm 12) ; mmの値範囲のチェック
        (<= 1 dd 31) ; ddの値範囲のチェック
        (year&month&day->datetime yyyy mm dd)))
    (error "invalid yyyymmdd" yyyymmdd)))


(define (fallback-filter-proc datetime target-day? current-month?)
  ;; 引数は以下
  ;;   - 該当日の<datetime>インスタンス
  ;;   - 指定日時の当日かどうか(真偽値)
  ;;   - 指定日時と同じ月かどうか
  ;;     (同じ月なら#t、カレンダー中の先月/来月の部分であれば#f)
  (and
    current-month?
    (date-day (datetime->date datetime))))


;; 指定されたdatetimeの月のカレンダーリストを生成する
(define (datetime->calendar-list . opts)
  (let-optionals* opts ((target-datetime (date->datetime (current-date)))
                        (filter-proc fallback-filter-proc))
    (let* (
           ;; target-monthは、指定月
           (target-month (date-month (datetime->date target-datetime)))
           (first-day-of-target-month (first-day-of-month
                                        (datetime->date target-datetime)))
           ;; prev-daysは、このカレンダーの先頭部分に含まれるべき
           ;; 先月分の日付が何日分あるのかを示す数値
           (prev-days (date-week-day first-day-of-target-month))
           ;; start-timeは、カレンダー作成を開始する、最初の日
           ;; (つまり、該当月の先月の最後の方の日曜、または該当月の一日目)
           (start-datetime (time->datetime
                             (subtract-duration
                               (date->time-utc first-day-of-target-month)
                               (make-time time-duration
                                          0
                                          (* prev-days 24 60 60)))))
           )
      (define (datetime-equal? datetime1 datetime2)
        (let ((date1 (datetime->date datetime1))
              (date2 (datetime->date datetime2)))
          (and
            (= (date-year date1) (date-year date2))
            (= (date-month date1) (date-month date2))
            (= (date-day date1) (date-day date2)))))
      (define (get-cur-week-result cur-datetime)
        ;; cur-datetimeからの七日間に対して、
        ;; filter-procを適用し、その結果をlistとして返す。
        ;; また、同時に、その次(つまり八日目)のdatetimeも多値で返す。
        (let loop ((days 0)
                   (cur-datetime cur-datetime)
                   (result '())
                   )
          (if (<= 7 days)
            (values (reverse result) cur-datetime)
            (loop
              (+ 1 days)
              (time->datetime
                (add-duration
                  (datetime->time cur-datetime)
                  *one-day-duration*))
              (cons
                (filter-proc cur-datetime
                             (datetime-equal? cur-datetime target-datetime)
                             (= target-month (date-month
                                               (datetime->date
                                                 cur-datetime))))
                result)))))

      (let loop-week ((cur-datetime start-datetime))
        (receive (cur-week-result next-datetime) (get-cur-week-result
                                                   cur-datetime)
          (if (= target-month
                 (date-month (datetime->date next-datetime)))
            (cons
              cur-week-result
              (loop-week next-datetime))
            (list cur-week-result)))))))



(define (prev-month-datetime datetime)
  (let1 date (datetime->date datetime)
    (date->datetime
      (if (= (date-month date) 1)
        (make-month 12 (- (date-year date) 1))
        (make-month (- (date-month date) 1) (date-year date))))))

(define (next-month-datetime datetime)
  (let1 date (datetime->date datetime)
    (date->datetime
      (if (= (date-month date) 12)
        (make-month 1 (+ (date-year date) 1))
        (make-month (+ (date-month date) 1) (date-year date))))))


(define (make-month m y)
  (make-date 0 0 0 0 1 m y *current-date-zone-offset*))

(define (first-day-of-month date)
  (make-month (date-month date) (date-year date)))





(provide "tir04/util/calendar")

