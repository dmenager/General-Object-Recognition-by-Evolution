(defstruct perceptron weights)

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
  (if (> (+ (weighted-sum (apply 'append img) p) b) 0)
      1
      0))

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
     do
       (setf (nth i (perceptron-weights p)) (+ weight-i (* (- ground a) l (nth i img))))))

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
(defun initialize-perceptron (img p)
  (setq (perceptron-weights p) 
	(mapcar #'(lambda (val)
		    (/ (random 101 (make-random-state t)) 100)))))
