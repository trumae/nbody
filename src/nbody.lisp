(defpackage nbody
  (:use :cl))
(in-package :nbody)

(require :sdl2)
(require :cl-opengl)

;;   The Computer Language Benchmarks Game
;;   http://benchmarksgame.alioth.debian.org/
;;;
;;; contributed by Patrick Frankenberger
;;; modified by Juho Snellman 2005-11-18
;;;   * About 40% speedup on SBCL, 90% speedup on CMUCL
;;;   * Represent a body as a DEFSTRUCT with (:TYPE VECTOR DOUBLE-FLOAT), a
;;;     not as a structure that contains vectors
;;;   * Inline APPLYFORCES
;;;   * Replace (/ DT DISTANCE DISTANCE DISTANCE) with
;;;     (/ DT (* DISTANCE DISTANCE DISTANCE)), as is done in the other
;;;     implementations of this test.
;;;   * Add a couple of declarations
;;;   * Heavily rewritten for style (represent system as a list instead of
;;;     an array to make the nested iterations over it less clumsy, use
;;;     INCF/DECF where appropriate, break very long lines, etc)
;;; modified by Marko Kocic 
;;;   * add optimization declarations

(declaim (optimize (speed 3)(safety 0)(space 0)(debug 0)))

(defconstant +days-per-year+ 365.24d0)
(defconstant +solar-mass+ (* 4d0 pi pi))

(defstruct (body (:type (vector double-float))
                 (:conc-name nil)
                 (:constructor make-body (x y z vx vy vz mass)))
  x y z
  vx vy vz
  mass)
(deftype body () '(vector double-float 7))

(defparameter *jupiter*
  (make-body 4.84143144246472090d0
             -1.16032004402742839d0
             -1.03622044471123109d-1
             (* 1.66007664274403694d-3 +days-per-year+)
             (* 7.69901118419740425d-3 +days-per-year+)
             (* -6.90460016972063023d-5  +days-per-year+)
             (* 9.54791938424326609d-4 +solar-mass+)))

(defparameter *saturn*
  (make-body 8.34336671824457987d0
             4.12479856412430479d0
             -4.03523417114321381d-1
             (* -2.76742510726862411d-3 +days-per-year+)
             (* 4.99852801234917238d-3 +days-per-year+)
             (* 2.30417297573763929d-5 +days-per-year+)
             (* 2.85885980666130812d-4 +solar-mass+)))

(defparameter *uranus*
  (make-body 1.28943695621391310d1
             -1.51111514016986312d1
             -2.23307578892655734d-1
             (* 2.96460137564761618d-03 +days-per-year+)
             (* 2.37847173959480950d-03 +days-per-year+)
             (* -2.96589568540237556d-05 +days-per-year+)
             (* 4.36624404335156298d-05 +solar-mass+)))

(defparameter *neptune*
  (make-body 1.53796971148509165d+01
             -2.59193146099879641d+01
             1.79258772950371181d-01
             (* 2.68067772490389322d-03 +days-per-year+)
             (* 1.62824170038242295d-03 +days-per-year+)
             (* -9.51592254519715870d-05 +days-per-year+)
             (* 5.15138902046611451d-05 +solar-mass+)))

(defparameter *sun*
  (make-body 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 +solar-mass+))

(declaim (inline applyforces))
(defun applyforces (a b dt)
  (declare (type body a b) (type double-float dt))
  (let* ((dx (- (x a) (x b)))
         (dy (- (y a) (y b)))
         (dz (- (z a) (z b)))
	 (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
	 (mag (/ dt (* distance distance distance)))
         (dxmag (* dx mag))
         (dymag (* dy mag))
         (dzmag (* dz mag)))
    (decf (vx a) (* dxmag (mass b)))
    (decf (vy a) (* dymag (mass b)))
    (decf (vz a) (* dzmag (mass b)))
    (incf (vx b) (* dxmag (mass a)))
    (incf (vy b) (* dymag (mass a)))
    (incf (vz b) (* dzmag (mass a))))
  nil)

(defun advance (system dt)
  (declare (double-float dt))
  (loop for (a . rest) on system do
        (dolist (b rest)
          (applyforces a b dt)))
  (dolist (b system)
    (incf (x b) (* dt (vx b)))
    (incf (y b) (* dt (vy b)))
    (incf (z b) (* dt (vz b))))
  nil)

(defun energy (system)
  (let ((e 0.0d0))
    (declare (double-float e))
    (loop for (a . rest) on system do
          (incf e (* 0.5d0
                     (mass a)
                     (+ (* (vx a) (vx a))
                        (* (vy a) (vy a))
                        (* (vz a) (vz a)))))
          (dolist (b rest)
            (let* ((dx (- (x a) (x b)))
                   (dy (- (y a) (y b)))
                   (dz (- (z a) (z b)))
                   (dist (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
              (decf e (/ (* (mass a) (mass b)) dist)))))
    e))

(defun offset-momentum (system)
  (let ((px 0.0d0)
	(py 0.0d0)
	(pz 0.0d0))
    (dolist (p system)
      (incf px (* (vx p) (mass p)))
      (incf py (* (vy p) (mass p)))
      (incf pz (* (vz p) (mass p))))
    (setf (vx (car system)) (/ (- px) +solar-mass+)
          (vy (car system)) (/ (- py) +solar-mass+)
          (vz (car system)) (/ (- pz) +solar-mass+))
    nil))

(defun nbody (n)
  (declare (fixnum n))
  (let ((system (list *sun* *jupiter* *saturn* *uranus* *neptune*)))
    (offset-momentum system)
    (format t "~,9F~%" (energy system))
    (dotimes (i n)
      (advance system 0.01d0))
    (format t "~,9F~%" (energy system))))


(defun debug-log (msg &rest args)
  "Output and flush MSG to STDOUT with arguments ARGS"
  (apply #'format t msg args)
  ;; Flush to standard out
  (finish-output))

(defun setup-gl (win gl-context)
  "Setup OpenGL with the window WIN and the gl context of GL-CONTEXT"
  (debug-log "Setting up window/gl.~%")
  (sdl2:gl-make-current win gl-context)
  (gl:viewport 0 0 800 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; Clear to black
  (gl:clear-color 0.0 0.0 0.0 1.0))


(defun draw-circle(radius x y c1 c2 c3)
  (let ((twincepi (* 2.0 pi))
	(lineAmount 100))
    (gl:begin :line-loop)
    (gl:color c1 c2 c3)
    (dotimes (i lineAmount)
      (gl:vertex
       (+ x (* radius (cos (* i (/ twincepi lineAmount)))))
       (+ y (* radius (sin (* i (/ twincepi lineAmount)))))))
    (gl:end)))

(defparameter *factorpos* 0.05)
(defparameter *factorradius* 0.3)
(defparameter *factorradiussun* 0.001)

(defun draw-body(body c1 c2 c3 factorradius factorpos)
  (draw-circle (* factorradius (mass body))
	       (* factorpos (x body))
	       (* factorpos (y body)) c1 c2 c3))

(defun render ()
  ;;;(gl:clear :color-buffer)
  (draw-body *jupiter* 0.0 1.0 0.0 *factorradius* *factorpos*)
  (draw-body *saturn* 0.0 0.0 1.0 *factorradius* *factorpos*)
  (draw-body *neptune* 1.0 1.0 0.0 *factorradius* *factorpos*)
  (draw-body *uranus* 0.0 1.0 1.0 *factorradius* *factorpos*)
  (draw-body *sun* 1.0 0.0 0.0 *factorradiussun* *factorpos*)
  (gl:flush))

(defun main-loop (win render-fn)
  "Run the game loop that handles input, rendering through the
  render function RENDER-FN, amongst others."
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
	   (funcall render-fn)
	   ;; Swap back buffer
	   (sdl2:gl-swap-window win))
    (:quit () t)))
 
(defun start-display ()
  "The entry point of our game."
  (bt:make-thread
   (lambda ()
     (sdl2:with-init (:everything)
       (debug-log "Using SDL library version: ~D.~D.~D~%"
		  sdl2-ffi:+sdl-major-version+
		  sdl2-ffi:+sdl-minor-version+
		  sdl2-ffi:+sdl-patchlevel+)
       
       (sdl2:with-window (win :flags '(:shown :opengl))
	 (sdl2:with-gl-context (gl-context win)
	   ;; Basic window/gl setup
	   (setup-gl win gl-context)
	   
	   ;; Run main loop
	   (main-loop win #'render)))))))
   

(defun start(n)
  (start-display)
  (nbody n))
