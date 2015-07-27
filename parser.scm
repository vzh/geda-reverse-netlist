;coding: utf-8
(use-modules (ice-9 lineio))
(use-modules (ice-9 rw))
(use-modules (ice-9 regex))
;(use-modules (geda page))

(add-to-load-path ".")
(load-from-path "define.scm")

(define (get-assignments-list inputf)
  (let* ((port (make-line-buffering-input-port (open-file inputf "r"))))
    (do ((line "" (read-string port))
         (regexp-list '() (add-to-regexp-list regexp-list line)))
      (
       ; test
       (eof-object? line)
       ; expressions to evaluate in the end
       (close-port port)
       ; return value
       regexp-list)
      ; empty body
      )
    ))

(define (add-to-regexp-list regexp-list line)
  (let ((line-list (string-split line (char-set #\space #\newline))))
    (if
      (and (pair? line-list) (not (null? (cdr line-list))))
      (if (null? regexp-list)
        (set! regexp-list
                  (list (cons (make-regexp (car line-list)) (cadr line-list))))
        (set! regexp-list
          (append regexp-list
                  (list (cons (make-regexp (car line-list)) (cadr line-list))))))
    )
    regexp-list))

; Регулярное выражение для пропуска пустых строк и строк,
; начинающихся с '#'
(define r-empty (make-regexp "^[ \t]*(#.*)*\n$"))
(define (regexp-empty? str)
  (regexp-exec r-empty str))

; Регулярное выражение для обозначения любых элементов (не должно
; включать пробельные символы и всё)
(define r-elem (make-regexp "[^ \t\n]+"))


; Добавление строки к списку соединений. Строка преобразуется в
; список подстрок
(define (add-to-netlist netlist line)
  (if (regexp-empty? line)
    netlist
    (append netlist
            (list (fold-matches
                    r-elem
                    line
                    '()
                    (lambda (m p) (append p (list (match:substring m))))
                    )))))

; Чтение файла и создание списка соединений на основе соединений
; (net-based netlist)
; В итоге должно быть: ((netname_1 (refdes_1 (pin_1 ... pin_n))) ... )
(define (parse-netbased-netlist inputf)
  (let ((port (make-line-buffering-input-port (open-file inputf "r"))))
    (do ((line "" (read-string port))
         (netlist '() (add-to-netlist netlist line)))
      (
       ; test
       (eof-object? line)
       ; expressions to evaluate in the end
       (close-port port)
       ; return value
       (cdr netlist))
       ; empty body
      )
    ))


(define (find-and-append ls net-record)
  (begin
  (if (null? ls)
    ; если список пуст, добавить запись целиком (netname (refdes pin))
    (set! ls (list ; новый список
               (cons ; пара
                 (car net-record)        ;netname
                 (list (cdr net-record)) ;(refdes pin)
                 )))
    ; иначе
    ; найти элемент, для которого netname совпадает с (car net-record)
    (if (equal? (car net-record) (caar ls))
      ; если найден, добавить подэлементы (refdes pin)
      (set! ls (append (list (append! (car ls) (list (cdr net-record)))) (cdr ls)))
      ; если не найден, смотрим следующую запись
      (set! ls (cons (car ls) (find-and-append (cdr ls) net-record)))))
  ls
  )
)

; Создание списка соединений на основе соединений
(define (join-nets old-list new-list)
(begin
  (if (null? old-list)
    ;new-list
    '()
    (begin
      (set! new-list (find-and-append new-list (car old-list)))
      ; продолжить со старым списком
      (set! new-list (join-nets (cdr old-list) new-list))
      )
  )new-list))


; Сравнение refdes
(define (refdes-equal? ls refdes)
  (string=? (car ls) refdes))


; объединение элементов списка с одинаковыми refdes вида
;   ((refdes1 (net1 pin1)) (refdes1 (net2 pin2)) ...)
; в вид
;   ((refdes1 (net1 pin1) (net2 pin2) ...))
(define (rework-instance-list! ls)
  (if
    (and
      (not (null? ls))
      (not (null? (cdr ls))))
    (begin
      (if
        ; если refdes двух элементов списка совпадают
        (equal? (caar ls) (caadr ls))
        (begin
          ; добавляем для следующего элемента в списке (netname pin) от предыдущего
          (set-car!
            (cdr ls)
            (append (cadr ls) (cdar ls)))
          ; 'del - пустой элемент для удаления
          (set-car! ls 'del)
          ))
      (rework-instance-list! (cdr ls)))
    ))

; выносим net и pin в подсписок для каждого refdes, то есть
; (refdes net pin) -> (refdes (net pin))
(define (separate-first ls)
  (map (lambda (x) (cons (car x) (list (cdr x)))) ls))

; Сортировка списка по позиционным обозначениям
(define (sort-by-first ls)
  (sort-list
      (map
        ; FIXME: заменить на swap-car-cadr
        (lambda (elem)
          (cons (cadr elem) (cons (car elem) (cddr elem))))
        ls)
      (lambda (x y) (string<? (car x) (car y)))))

; Создание списка соединений на основе компонентов
(define (join-by-first initial-netlist)
  (let ((ls (separate-first (sort-by-first initial-netlist))))
    ; деструктивно меняем список
    (rework-instance-list! ls)
    ; уничтожаем пустые элементы и возвращаем
    (filter (lambda (x) (not (eq? x 'del))) ls)
  ))

; автозаполнение отсутствующих pin для одного refdes
(define (auto-complete-pin/element! refdes)
  ; (cdr refdes) == ((net1 pin1) ... (netN pinN))
  (let ((newls
          (map-in-order
    (lambda (x)
      (if (null? (cdr x))
        (cons (car x) (cons 'unnamed '()))
        x))
    (cdr refdes))))
    (cons (car refdes) newls)
    )
  )

; автозаполнение отсутствующих pin в списке соединений на основе экземпляров
(define (auto-complete-pins! ls)
  (let ((newls
  (map-in-order
    (lambda (x) (auto-complete-pin/element! x))
    ls)))
    newls))

; неименованный вывод?
(define (pin-is-unnamed? pin)
  (eq? pin 'unnamed))

; задать новое имя вывода
(define (set-uniq-pin-name! net-pin-pair newpinname)
  ; новый список вида (netname pinname)
  (cons (car net-pin-pair) (cons newpinname '())))

(define (get-uniq-pin-name namelist)
  (let inlist ((newnum 1))
    (if (member (number->string newnum) namelist)
      (inlist (1+ newnum))
      (number->string newnum)))
  )

; переименование 'unnamed для безымянных выводов
(define (rename-unnamed! ls)
  (map
    (lambda (refdes-list)
      (cons (car refdes-list)
            (let (
            (namelist (map (lambda (x) (cadr x)) (cdr refdes-list))) ; список имеющихся имён
            )
      (map
        (lambda (net-pin-pair)
          (if
            (pin-is-unnamed? (cadr net-pin-pair))
            (let ((uniq-name (get-uniq-pin-name namelist)))
              (append! namelist (list uniq-name) )
              (cons
                (car net-pin-pair)
                (cons uniq-name '())
                )
              )
            net-pin-pair)
           )
        (cdr refdes-list))
      ))
      )
    ls))


(define initial-netlist (parse-netbased-netlist "netlist"))

(define net-netlist '())


; FIXME: эта штука вроде больше не используется, надо убрать
(define initial-netbased-netlist
  (join-nets initial-netlist net-netlist))

(define instancebased-netlist
  (rename-unnamed!
    (auto-complete-pins!
      (join-by-first initial-netlist))))

(define (get-instance-symbol-name refdes assignments-list)
  (if (null? assignments-list)
    "resistor-1.sym"
    (if (regexp-exec (caar assignments-list) refdes)
      (cdar assignments-list) (get-instance-symbol-name refdes (cdr assignments-list)))
    ))

(define (instance->string ls assignments-list x y)
  (append-component-with-attribs
          (get-instance-symbol-name (car ls) assignments-list)
          (cons x y)
          (car ls)))

(define (netlist->schematic ls)
  (begin

    ; start

    (let* ((assignments-list (get-assignments-list "assignments"))
          (N (length ls))
          (pi 3.1415926)
          ; distance between components
          (dist 1000)
          (R (/ (* dist N) 2 pi))
          ; angle delta to increase the angle
          (delta-alpha (/ (* 2 pi) N))
          (alpha 0)
          ; center coords
          (xc 40000)
          (yc 40000)
          (x  0)
          (y  0)
          (num 0)
          )
      (for-each (lambda (el) (begin
                               (set! num (1+ num))
                               (set! alpha (* num delta-alpha))
                               (set! x (inexact->exact (* 100 (round (/ (+ xc (* R (cos alpha))) 100)))))
                               (set! y (inexact->exact (* 100 (round (/ (+ yc (* R (sin alpha))) 100)))))
                               (instance->string el assignments-list x y))) ls)
      )


    ; add nets
    (netbased-netlist->schematic-nets netbased-netlist)

    ; finish
    ))

; (A (B C) (D E) ...) => ((A B C) (A D E) ...)
(define (apap ls)
  (let ap ((l ls))
    (append (car l)
            (if (null? (cdr l)) '() (ap (cdr l)))))
  )

; (A (B C) (D E) ...) => ((A B C) (A D E) ...)
(define (flatten-one ls)
  (map (lambda (x) (map (lambda (y) (cons (car x) y)) (cdr x))) ls)
  )

; ((A B C) (D E F) ... (G H I)) -> ((B A C) (E D F) ... (H G I))
; FIXME: см. выше, где и что надо заменить на эту процедуру
(define (swap-car-cadr ls)
  (map (lambda (x) (cons (cadr x) (cons (car x) (cddr x)))) ls)
  )

(define netbased-netlist
  (join-by-first (apap (flatten-one instancebased-netlist))))

(define (net->string ls)
  (if (not (null? (cdr ls)))
    (begin
      ; (append-net pair1 pair2)
      (append-net (cons (caar ls) (cadar ls)) (cons (caadr ls) (cadadr ls)))
      (net->string (cdr ls))
      )
    ))

(define (netbased-netlist->schematic-nets ls)
  (for-each (lambda (net) (net->string (cdr net))) ls)
)


;; Main program
(netlist->schematic instancebased-netlist)
