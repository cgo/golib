#!/home/christian/Software/bin/guile -s 
!#
(primitive-load "./golib_guile.scm")
(use-modules (oop goops describe))
; (use-modules (golib_guile))
(define sig (make-instance <gosignal3d>))
(if (entity? sig)
  (format #t "This is an entity.~%")
  (format #t "This is not an entity.~%"))
(describe sig)
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
(define siglist (list (make-instance <gosignal3d>) 
                      (make-instance <gosignal3d>) 
                      (make-instance <gosignal3d>) 
                      (make-instance <gosignal3d>)))

(describe (cadr siglist))

;; Test reading an image:
(primitive:goFileIO-readImage "/home/christian/KDE.jpg" sig)
;; Make a new signal and set to float type:
(define sig2 (make-instance <gosignal3d>))
(setDataType sig2 (GO-FLOAT))
;; Allocate it:
(make sig2 (getSizeX sig) (getSizeY sig) (getSizeZ sig))
;; Convert first image to scalar float:
(if (goRGBAtoScalar sig sig2)
  (format #t "Successfully converted.~%")
  (format #t "Conversion failed.~%"))
;; Try to write the converted image:
(if (primitive:goFileIO-writeImage "caro4-scalar.jpg" sig2)
  (format #t "Written.~%")
  (format #t "Write failed.~%"))
