(in-package :play-with-fond2)

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

(defstruct (text (:constructor %make-text))
  (varr (error "missing varr") :type gpu-array)
  (iarr (error "missing iarr") :type gpu-array)
  (stream (error "missing stream") :type buffer-stream)
  (font (error "missing font") :type t))

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

(defun make-text (font &optional (text ""))
  (check-type text string)
  (let* ((varr (make-gpu-array nil :dimensions 1 :element-type 'texticle))
         (iarr (make-gpu-array nil :dimensions 1 :element-type :uint))
         (stream (make-buffer-stream varr :index-array iarr))
         (obj (%make-text :varr varr :iarr iarr :stream stream :font font)))
    (when text
      (update-text obj text))
    obj))

(defun update-text (text-obj text-str)
  (let ((new-len
         (cl-fond:update-text
          (text-font text-obj)
          text-str
          (gpu-buffer-id (gpu-array-buffer (text-varr text-obj)))
          (gpu-buffer-id (gpu-array-buffer (text-iarr text-obj))))))
    (setf (buffer-stream-length (text-stream text-obj))
          new-len)
    text-obj))

(defun test-draw-text (text-obj)
  (let ((res (viewport-resolution (current-viewport))))
    (clear)
    (map-g #'fondness (text-stream text-obj)
           :extent (v! 0f0 0f0 (x res) (y res))
           :tex-image *sampler*
           :text-color (v! 1 1 1 1))
    (swap)
    text-obj))

;;------------------------------------------------------------
;; test
;;
;; > (cepl:repl)
;; > (setup)
;; > (defvar tmp0 (make-text *font* "heya!"))
;; > (test-draw-text tmp0)
;; > (update-text tmp0 "HORSE HORSE HOORRRSE")
;; > (test-draw-text tmp0)
