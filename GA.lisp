(load "perceptron")

(defun create-creature ()
  
  )

#| Genetic algorithm to evolve eco-features |#
;; size-pop = size of population
;; num-generations = number of generations to create
;; training-set = training images
;; holding-set = holdout set of images
(defun optimize-features (size-pop num-generations training-set holding-set)
  (let (transforms candidate-feats eco-feats)
    ;; leaving out optional params
    (setq transoforms
	  (list '(:name gabor :num-params 2 :types '(img float))
		'(:name gradient :num-params 2 :types '(img int))
		'(:name square-root :num-params 1 :types '(img))
		'(:name gaussian :num-params 1 :types '(img))
		'(:name histogram :num-params 2 :types '(img int))
		'(:name hough-circle :num-params 3 :types '(img float bool))
		'(:name normalize :num-params 0 :types '(())) ;; Ask Dr. Wang about this
		'(:name convert :num-params 0 :types '(())) ;; Ask Dr. Wang about this
		'(:name median-blur :num-params 2 :types '(img int))
		'(:name integral-blur :num-params 1 :types '(img))
		'(:name canny-edge :num-params 4 :types '(img float float float))
		'(:name rank-order :num-params 1 :types '(img))
		'(:name resize :num-params 1 :types '(img))
		'(:name log :num-params 0 :types '(())) ;; Ask Dr. Wang about this
		'(:name sobel :num-params 1 :types '(img))
		'(:name DoG :num-params 0 :types '(())) ;; Ask Theodore about this
		'(:name erosion :num-params 2 :types '(img int))
		'(:name threshold-adaptive :num-params 3 :types '(img int (generic gaussian mean median)))
		'(:name hough-line :num-params 1 :types '(img))
		'(:name fourier :num-params 0 :types '(())) ;; Ask Theodore about this
		'(:name equalize-hist :num-params 2 :types '(img int))
		'(:name laplace-edge :num-params 2 :types '(img int))
		'(:name distance-transform :num-params 0 :types '(()) ;; Ask Dr. Wang about this
		'(:name dilation :num-params 1 :types '(img))
		'(:name corner-harris :num-params 5 :types '(img (k eps) float float float))
		'(:name census-transform :num-params 0 :types '(())) ;; Ask Dr. Wang about this
		'(:name pixel-statistics :num-params 0 :types '(())) ;; Ask Dr. Wang about this
		))
    (loop
       for i from 0 to (- size-pop 1)
       do
	 (push (create-creature) candidate-feats))
    (loop
       for i from 0 to (- num-generations 1)
       do
	 (loop
	    for creature in candidate-feats
	    do
	      (loop 
		 for img in training
		 do
		   (train creature img))
	      (loop
		 for hold in holding
		 with fitness = 0 and tp = 0 and tn = 0 and fp = 0 and fn = 0
		 do
		   (setq fitness (fitness creature hold))
		 finally 
		   ;; set creatures fitness
		   ;; if creature's fitness > thresh, then save
		   ))
	 (generate-next-generation)))
