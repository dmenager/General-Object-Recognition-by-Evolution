(load "perceptron")

#| Genetic algorithm to evolve eco-features |#
;; size-pop = size of population
;; num-generations = number of generations to create
;; training-set = training images
;; holding-set = holdout set of images
(defun optimize-features (size-pop num-generations training-set holding-set)
  (let (transforms)
    (setq transoforms
	  (list '(:name gabor :num-params 0 :types-ranges '(()))
		'(:name gradient :num-params  :types-ranges '(()))
		'(:name square-root :num-params 0 :types-ranges '(()))
		'(:name gaussian-blur :num-params 0 :types-ranges '(()))
		'(:name histogram :num-params 0 :types-ranges '(()))
		'(:name hough-circle :num-params 0 :types-ranges '(()))
		'(:name normalize :num-params 0 :types-ranges '(())) ;; Ask Dr. Wang about this
		'(:name convert :num-params 0 :types-ranges '(())) ;; Ask Dr. Wang about this
		'(:name median-blur :num-params 0 :types-ranges '(()))
		'(:name integral-blur :num-params 0 :types-ranges '(()))
		'(:name canny-edge :num-params 0 :types-ranges '(()))
		'(:name rank-transform :num-params 0 :types-ranges '(())) ;; Ask Dr. Wang about this
		'(:name resize :num-params 0 :types-ranges '(()))
		'(:name log :num-params 0 :types-ranges '(())) ;; Ask Dr. Wang about this
		'(:name sobel :num-params 0 :types-ranges '(()))
		'(:name DoG :num-params 0 :types-ranges '(())) ;; Ask Theodore about this
		'(:name morphological-erode :num-params 0 :types-ranges '(()))
		'(:name threshold-adaptive :num-params 0 :types-ranges '(()))
		'(:name hough-line :num-params 0 :types-ranges '(()))
		'(:name fourier :num-params 0 :types-ranges '(())) ;; Ask Theodore about this
		'(:name equalize-hist :num-params 0 :types-ranges '(()))
		'(:name laplace-edge :num-params 0 :types-ranges '(()))
		'(:name distance-transform :num-params 0 :types-ranges '(())) ;; Ask Dr. Wang about this
		'(:name dilation :num-params 0 :types-ranges '(()))
		'(:name corner-harris :num-params 0 :types-ranges '(()))
		'(:name census-transform :num-params 0 :types-ranges '(())) ;; Ask Dr. Wang about this
		'(:name pixel-statistics :num-params 0 :types-ranges '(())) ;; Ask Dr. Wang about this
		))
    ))
