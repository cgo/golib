#!/usr/bin/guile -s 
!#
(primitive-load-path "golib_guile.scm")
(use-modules (oop goops describe))
; (use-modules (golib_guile))

;(define mylist (golib-test-list))
;(format #t "~S~%" mylist)
;(golib-test-list-2 (list 2 6 4.0 (/ 1 6)))
 
(define sig (make <goSignal3Dv>))
(format #t "Class name: ~a~%Object name: ~a~%" 
        (getClassName sig) 
        (toCharPtr (getObjectName sig)))
(if (string=? (getClassName sig) "goSignal3D")
  (format #t "This is a signal!~%")
  (format #t "This is not a signal!~%"))

(if (setDataType sig (GO-FLOAT))
  (format #t "Successfully set to float~%")
  (format #t "AAARGH~%"))

(if (HAVE-LIBIL)
  (format #t "Have libIL~%")
  (format #t "Don't have libIL~%"))

;; Make a list of signals:
(define siglist (list (make-instance <goSignal3Dv>) 
                      (make-instance <goSignal3Dv>) 
                      (make-instance <goSignal3Dv>) 
                      (make-instance <goSignal3Dv>)))

;; Test reading an image:
(if (primitive:goFileIO-readImage "/home/gosch/Documents/fem-level-sets/matlab/person.jpg" sig)
  (format #t "Read image.~%")
  (begin
    (format #t "Could not read image.~%")
    (quit)))
;; Make a new signal and set to float type:
(define sig2 (make-instance <goSignal3Dv>))
(setDataType sig2 (GO-UINT8))
;; Allocate it:
(make-signal sig2 (getSizeX sig) (getSizeY sig) (getSizeZ sig))
;; Convert first image to scalar float:
(if (goRGBAtoScalar sig sig2)
  (format #t "Successfully converted.~%")
  (format #t "Conversion failed.~%"))
;; Try to write the converted image:
(if (primitive:goFileIO-writeImage "scalar-image.jpg" sig2)
  (format #t "Written.~%")
  (format #t "Write failed.~%"))
