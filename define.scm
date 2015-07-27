(use-modules (geda page))
(use-modules (geda object))

(define (output-error error-type func-name message culprit)
  (format (current-error-port) "ERROR in ~A: ~A\n" func-name message)
  (scm-error error-type func-name message (list culprit) '()))

(define (remove-nets)
  (let ((page (active-page))
        (nets (filter net? (page-contents (active-page)))))
    (apply page-remove! page nets)))

; filter objects by given refdes
(define (get-objects-by-refdes refdes)
  (filter
    (lambda (x) (has-refdes? x refdes))
    (page-contents (active-page))))

; filter objects by given pinnumber
(define (get-objects-with-refdes-by-pinnumber refdes pinnumber)
  (let* ((objects (get-objects-by-refdes refdes))
         (found (filter
                  (lambda (object)
                    (get-pin-with-number (component-pin-list object) pinnumber))
                  objects)))
    (if (or
          (> (length found) 1)
          (null? found))
      (error "get-objects-by-pinnumber: Too many or too few objects with given \"pinnumber\":" found)
      (car found))))

; fixme: объединить has-refdes? и has-pinnumber-in-question?
; можно даже просто (get-attrib object attrib), и если его значение будет #f,
; значит такого атрибута нет
(define (has-refdes? object refdes)
  (not
    (null?
      (filter
        (lambda (attr)
          (and (equal? (attrib-name attr) "refdes") (equal? (attrib-value attr) refdes)))
        (object-attribs object)))))

(define (has-pinnumber-in-question? object pinnumber)
  (not
    (null?
      (filter
        (lambda (attr)
          (and (equal? (attrib-name attr) "pinnumber") (equal? (attrib-value attr) pinnumber)))
        (object-attribs object)))))

(define (get-pin-with-number pin-list pinnumber)
  (let ((pins-with-number (filter (lambda (pin) (has-pinnumber-in-question? pin pinnumber)) pin-list)))
    (if (> (length pins-with-number) 1)
      (output-error 'pin-number-error "get-pin-with-number"
                    (format #f "Pins ~A have the same \"pinnumber\"" pins-with-number) pins-with-number)
      (if (null? pins-with-number)
        (output-error 'pin-number-error "get-pin-with-number"
                      (format #f "No pins with \"pinnumber=~A\"" pinnumber) pinnumber)
        (car pins-with-number)))))

(define (component-pin-list object)
  (if (component? object)
    (filter pin? (component-contents object))
    (output-error 'misc-error "component-pin-list"
                  (format #f "Object ~A is not component" object) object)
    ))

; get pin coord for pinnumber
(define (get-object-pin-coord object pinnumber)
  ;line-start is the connectible point
  (line-start (get-pin-with-number
              (component-pin-list object)
              pinnumber)))

; make net between two fignations: (refdes1 . pinnumber1) and (refdes2 . pinnumber2)
(define (make-net-between-refdes-pinnumber-pairs pair1 pair2)
  (let ((refdes1 (car pair1))
        (pinnumber1 (cdr pair1))
        (refdes2 (car pair2))
        (pinnumber2 (cdr pair2)))
    (make-net
      (get-object-pin-coord
        (get-objects-with-refdes-by-pinnumber refdes1 pinnumber1)
        pinnumber1)
      (get-object-pin-coord
        (get-objects-with-refdes-by-pinnumber refdes2 pinnumber2)
        pinnumber2)
      )))

(define (append-component-with-attribs symbol-name coords refdes)
  (let ((C (make-component/library symbol-name coords 0 #f #f))
        (A (make-text coords 'middle-center 0 (string-append "refdes=" refdes) 12 #t 'value)))
    (if C
      (begin (page-append! (active-page) C A)
             (attach-attribs! C A))
      (output-error 'misc-error "append-component-with-attribs"
                    (format #f "Component ~A not found" symbol-name) symbol-name)
      )))

(define (append-net pair1 pair2)
  (page-append!
    (active-page)
    (make-net-between-refdes-pinnumber-pairs pair1 pair2)))
