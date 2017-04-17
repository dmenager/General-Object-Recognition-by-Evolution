(defstruct perceptron weights)
#| Set the weights of the perceptron |#

;; weights = newly learned weights for the first M inputs
;; perceptron-weights = remainder weights from [M+1, N]
(defun set-weights (weights perceptron-weights)
  (dolist (new-weight (reverse weights))
    (setq perceptron-weights (cons new-weight perceptron-weights)))
  perceptron-weights)

(defun flatten (x &optional stack out)
  (cond ((consp x) (flatten (rest x) (cons (first x) stack) out))
        (x         (flatten (first stack) (rest stack) (cons x out)))
        (stack     (flatten (first stack) (rest stack) out))
        (t out)))

#| Perform weighted sum of classifiyer |#

;; xis = inputs to perceptron
;; p = perceptron
(defun weighted-sum (xis p)
  (reduce '+ (mapcar '* (perceptron-weights p) xis)))

#| Generate output class from perceptron |#
;; img = input image
;; p = perceptron
;; b = bias term
;; stream = output stream
(defun classify (img p b stream)
  (let (weighted)
    (setq weighted (weighted-sum img p))
    (format stream "Weighted sum + bias: ~d~%~%" (+ weighted b))
    (if (> (+ weighted b) 0)
	1
	0)))

#| Update the perceptron weights |#
;; l = learning rate
;; a = perceptron output
;; ground = image class
;; p = perceptron
;; img = input image
(defun update-weights (l a ground p img)
  (let (update)
    (setq update (mapcar #'(lambda (img-pix) (* l (- ground a) img-pix)) img))
    (mapcar '+ (perceptron-weights p) update)))

#| Train the perceptron |#

;; p = perceptron
;; img = input image
;; predicted = output of perceptron
;; learning-rate = learning rate
(defun train (p img predicted learning-rate)
  (update-weights learning-rate
		  predicted
		  (getf img :label)
		  p
		  (getf img :image)))

#| Calculate fitness of perceptron [0, 1000) |#
;; tp = true positive
;; tn = true negative
;; fp = false positive
;; fn = false negative
;; p = penalty
(defun calculate-fitness (tp tn fp fn p)
  (setq p 0)
  (let (accuracy precision recall precision-defined recall-defined)
    (setq precision-defined t)
    (setq recall-defined t)
    (cond ((= 0 tp fp)
	   (setq precision-defined nil)
	   (setq precision 0))
	  ((= 0 tp fn)
	   (setq recall-defined nil)
	   (setq recall 0)))
    (setq accuracy (/ (+ tp tn) (+ tp tn fp fn)))
    (when recall-defined
      (setq recall (/ tp (+ tp fn))))
    (when precision-defined
      (setq precision (/ tp (+ tp fp))))
    (if (and (= recall 1) (< precision 1))
	;; I've guessed only the positive class
	0
	(float (* 500 (+ accuracy precision recall))))
    #|
    (* 500 (+ (* alpha accuracy)
	      (* (- 1 alpha) (+ precision recall))))|#))

#| Initialize perceptron weights |#

;; img = image to initialize perceptron
;; p = perceptron
(defun initialize-perceptron (num-inputs p)
  (setf (perceptron-weights p)
	(loop
	   for i from 0 to num-inputs
	   collect (/ (random 101) 100))))

#| Initialize perceptron weights |#

;; img = image to initialize perceptron
;; p = perceptron
(defun initialize-perceptron-2 (num-inputs p)
  (setf (perceptron-weights p)
	(loop
	   for i from 0 to num-inputs
	   collect 0)))

#|
SCRATCH
(let (flattened p)
  (setq p
	(sb-ext:run-program "python"
			    (list (concatenate 'string
					 (sb-unix::posix-getenv "HOME")
					 "/Code/courses/eecs_741/General-Object-Recognition-by-Evolution/imageProcessing.py")			      
			    "/home/david/Code/courses/eecs_741/256_ObjectCategories/002.american-flag/002_0001.jpg")
		      :search t
		      :wait nil
		      :output :stream))
  (setq flattened (read (process-output p)))
  flattened)

(let (p) (setq p (make-perceptron)) (initialize-perceptron 10 p))
|#
