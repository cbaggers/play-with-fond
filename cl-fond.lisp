(in-package :play-with-fond)

;;------------------------------------------------------------

(defvar *font* nil)
(defvar *sampler* nil)

;;------------------------------------------------------------

(defstruct-g texticle
  (position :vec2)
  (in-tex-coord :vec2))

(defun-g calc-text-uvs ((position :vec2)
                        (in-tex-coord :vec2)
                        (extent :vec4))
  (vec4 (* 2 (/ (+ (x position) (x extent))
                (- (z extent) 1.0)))
        (* 2 (/ (- (y position) (y extent))
                (+ (w extent) 1.0)))
        0.0
        1.0))

(defun-g tvert ((text-info texticle) &uniform (extent :vec4))
  (with-slots (position in-tex-coord) text-info
    (values
     (calc-text-uvs position in-tex-coord extent)
     in-tex-coord)))

(defun-g tfrag ((tex-coord :vec2)
                &uniform
                (tex-image :sampler-2d)
                (text-color :vec4))
  (let ((intensity (x (texture tex-image tex-coord))))
    (* (vec4 intensity) text-color)))

(defpipeline-g fondness ()
  (tvert texticle)
  (tfrag :vec2))

;;------------------------------------------------------------

(defun setup ()
  (setf *font*
        (cl-fond:make-font
         #p"/home/baggers/Downloads/Roboto-Regular(2).ttf"
         "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ .!?"))
  (setf *sampler*
        (sample
         (make-texture-from-id (cl-fond:texture *font*)
                               :base-dimensions '(? ?)
                               :element-type :rgba8))))

(defun compute-text-stream (string)
  (multiple-value-bind (vao num-of-elements)
      (cl-fond:compute-text *font* string)
    (make-buffer-stream-from-id-and-layouts
     vao
     `((:element-type texticle :dimensions ?))
     `(:element-type :uint :dimensions ,num-of-elements))))

(defun draw-text-stream (text-stream)
  (let ((res (viewport-resolution (current-viewport))))
    (clear)
    (map-g #'fondness text-stream
           :extent (v! 0f0 0f0 (x res) (y res))
           :tex-image *sampler*
           :text-color (v! 1 1 1 1))
    (swap)
    text-stream))

(defun draw-text (&optional (string "Heyo!"))
  (draw-text-stream (compute-text-stream string)))

;;------------------------------------------------------------
;; test
;;
;; > (cepl:repl)
;; > (setup)
;; > (defvar tmp0 (compute-text-stream "yo mama"))
;; > (draw-text-stream tmp0)
