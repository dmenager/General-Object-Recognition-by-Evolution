(defstruct perceptron weights)

(defun py-flatten (img)
  (let (flattened p)
  (setq p
	(sb-ext:run-program "python"
		      (list (concatenate 'string
					 (sb-unix::posix-getenv "HOME")
					 "/Code/courses/eecs_741/General-Object-Recognition-by-Evolution/imageProcessing.py")			      
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
  ;; call python to return a list of lists (img)
  (let (flat-image)
    (setq flat-image (py-flatten img))
    (if (> (+ (weighted-sum flat-image p) b) 0)
	1
	0)))

#| Update the perceptron weights |#
;; l = learning rate
;; a = perceptron output
;; ground = image class
;; p = perceptron
;; img = input image
(defun update-weights (l a ground p img)
  (let (flat-image)
    (setq flat-image (py-flatten img))
    (loop
       for i from 0 to (- (length (perceptron-weights p)) 1)
       with weight-i = (nth i (perceptron-weights p))
       do
	 (setf (nth i (perceptron-weights p)) (+ weight-i (* (- ground a) l (nth i flat-img)))))))
  
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
	   collect (/ (random 101 (make-random-state t)) 100))))
#|
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
|#
