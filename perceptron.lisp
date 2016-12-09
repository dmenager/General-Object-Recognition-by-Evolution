(defstruct perceptron weights)

(defun py-flatten (img)
  (let (flattened p string-stream)
    (setq string-stream make-string-output-stream)
    (write-string (sb-unix::posix-getenv "HOME") string-stream)
    (write-string "/Code/courses/eecs_741/General-Object-Recognition-by-Evolution/imageProcessing.py")
    (setq p
	  (sb-ext:run-program "python"
			      (list (get-output-stream-string string-stream)			      
				    img)
			      :search t
			      :wait nil
			      :output :stream))
    (setq flattened (read (process-output p)))
    flattened))

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
(defun classify (img p b)
  (if (> (+ (weighted-sum img p) b) 0)
      1
      0)))

#| Update the perceptron weights |#
;; l = learning rate
;; a = perceptron output
;; ground = image class
;; p = perceptron
;; img = input image
(defun update-weights (l a ground p img)  
  (loop
     for i from 0 to (- (length (perceptron-weights p)) 1)
     with weight-i = (nth i (perceptron-weights p))
     collect (+ weight-i (* (- ground a) l (nth i img))) into weights
     finally (return weights)))

#| Train the perceptron |#

;; p = perceptron
;; img = input image
;; learning-rate = learning rate
(defun train (p img learning-rate)
  (update-weights learning-rate
		  (classify (getf img :image) p 1)
		  (getf img :label)
		  p
		  (getf img :img)))

#| Calculate fitness of perceptron |#
;; tp = true positive
;; tn = true negative
;; fp = false positive
;; fn = false negative
(defun calculate-fitness (tp tn fp fn)
  (+ (/ (* tp 500) (+ fn tp)) 
     (/ (* tn 500) (+ fp tn))))

#| Initialize perceptron weights |#

;; img = image to initialize perceptron
;; p = perceptron
(defun initialize-perceptron (num-inputs p)
  (setf (perceptron-weights p)
	(loop
	   for i from 0 to num-inputs
	   collect (/ (random 101) 100))))

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
