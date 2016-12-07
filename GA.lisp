(load "perceptron")

#| Shuffle list |#

(defun random-shuffle (sequence)
  (map-into sequence #'car
            (sort (map 'vector (lambda (x)
                                 (cons x (random 1d0)))
                       sequence)
                  #'< :key #'cdr)))

#| Define a random number in range [start, end] |#

(defun rand-in-range (start end)
  (+ start (random (+ 1 (- end start)) (make-random-state t))))

#| Read images from directory into memory |#
(defun read-images (img-dirs)
  (let (imgs cip)
    (loop
       for img-dir in img-dirs
       for i from 0 to (- (length img-dirs) 1)
       collect (cons i (second img-dir)) into category-index-pairs      
       do
	 (loop
	    for image in (directory (first img-dir))
	    collect (list ':image image ':label i) into images
	    finally (setq imgs (nconc imgs images)))
       finally
	;; (format t "images:~%~A~%cip:~%~A~%" images category-index-pairs)
	 (setq cip category-index-pairs))
    (values imgs cip)))

#| Convert eco feature to python string of lists |#

;; feats = eco-feature ((x1 x2 y1 y2) (f1 params) (f2 params) ... (fn params))
(defun make-str-features (feats)
  (let (string-stream)
    (setq string-stream (make-string-output-stream))
    ;; add outer layer list
    (write-string "[" string-stream)
    (mapcar #'(lambda (feature)
		(write-string "[" string-stream)
		(loop
		   for arg in feature
		   do
		     (if (or (numberp arg) (symbolp arg))
			 (write-string (write-to-string arg) string-stream)
			 (write-string arg string-stream))
		     (when (not (= (position arg feature)
				   (- (length feature) 1)))
		       (write-string ", " string-stream)))
		(write-string "]" string-stream))
	    feats)
    (write-string "]" string-stream)
    (get-output-stream-string string-stream)))

#| Apply transformations to image, as described by eco-feature |#

;; transforms = eco-feature in python string of list
;; img = image to apply eco-features to
(defun pre-process-image (transforms img)
  (sb-ext:run-program "python"
		      (list (concatenate 'string
					 (sb-unix::posix-getenv "HOME")
					 "/Code/courses/eecs_741/General-Object-Recognition-by-Evolution/imageTransforms.py")			      
			    img
			    transforms)
		      :search t
		      :wait nil
		      :output :stream))

#| Train the perceptron |#
(defun train (perceptron img))

(defun create-transforms ()
  (let (transforms num-transforms)
    ;; leaving out optional params
    ;; scikit-image transforms
    (setq transforms
	  (list '(:name gabor :types (img float))
		'(:name gradient :types (img int))
		;;'(:name square-root :types (img)) 
		'(:name gaussian :types (img))
		'(:name histogram :types (img int))
		'(:name hough-circle :types (img float bool))
		;;'(:name normalize :num-params 0 :types '(())) ;; Ask Dr. Wang about this
		;;'(:name convert :num-params 0 :types '(())) ;; Ask Dr. Wang about this
		'(:name median-blur :types (img int))
		'(:name integral-blur :types (img))
		'(:name canny-edge :types (img float float float))
		'(:name rank-order :types (img))
		;;'(:name resize :types (img)) ;; implement size tuple
		;;'(:name log :num-params 0 :types '(())) ;; Ask Dr. Wang about this
		'(:name sobel :types (img))
		;;'(:name DoG :num-params 0 :types '(())) ;; Ask Theodore about this
		'(:name erosion :types (img int))
		'(:name threshold-adaptive :types (img int ("generic" "gaussian" "mean" "median")))
		'(:name hough-line :types (img))
		;;'(:name fourier :num-params 0 :types '(())) ;; Ask Theodore about this
		'(:name equalize-hist :types (img int))
		'(:name laplace-edge :types (img int))
		;;'(:name distance-transform :types '(()) ;; Ask Dr. Wang about this
		'(:name dilation :types (img int))
		;;'(:name corner-harris :types (img ("k" "eps") float float float)) ;; investigate this later
		;;'(:name census-transform :types '(())) ;; Ask Dr. Wang about this
		;;'(:name pixel-statistics :types '(())) ;; Ask Dr. Wang about this
		))
    (setq num-transforms (random (length transforms) (make-random-state t)))
    (format t "Selected number of transforms: ~d~%" num-transforms)
    (loop
       for i from 1 to num-transforms
       with cur-transform = nil and transform-types = nil and transform-name = nil
       do
	 (setq cur-transform (nth (random (length transforms) (make-random-state t)) transforms))
	 (setq transform-types (getf cur-transform :types))
	 (setq transform-name (getf cur-transform :name))
	 (loop
	    for type in transform-types
	    with type-vals = nil
	    do
	      (cond
		((equal 'img type))
		((equal 'float type)
		 (push (random 100.00 (make-random-state t)) type-vals))
		((equal 'int type)
		 (push (random 100 (make-random-state t)) type-vals))
		((listp type)
		 (push (nth (random (length type) (make-random-state t)) type) type-vals))
		((equal 'bool type)
		 (push (nth (random 2 (make-random-state t)) '(t nil)) type-vals))
		(t (error "Unsupported transform type: ~A" type)))
	    finally (setq transform-types (reverse type-vals)))
	 (push transform-name transform-types)
       collect transform-types into eco-transforms
       finally (return eco-transforms))))

(defun create-creature ()
  (let (max-width max-height x1 x2 y1 y2 perceptron feats region)
    (setq max-width 1000)
    (setq max-height 1000)

    (format t "Defining ROI.~%")
    (setq x1 (rand-in-range 0 (- max-width 2)))
    (setq x2 (rand-in-range (+ 1 x1) (- max-width 1)))
    (setq y1 (rand-in-range 0 (- max-height 2)))
    (setq y2 (rand-in-range (+ 1 y1) (- max-height 1)))
    (setq region (list x1 x2 y1 y2))
    (format t "Creating perceptron.~%")
    (setq perceptron (make-perceptron))
    (format t "Initializing weights.~%")
    (initialize-perceptron (* (- x2 x1) (- y2 y1)) perceptron)
    (format t "Creating Transformations.~%")
    (setq feats (create-transforms))    
    (list ':features (cons region feats) ':perceptron perceptron)))

#| Genetic algorithm to evolve eco-features |#
;; size-pop = size of population
;; num-generations = number of generations to create
;; dirs = list of images
(defun evolve-features (size-pop num-generations dirs)
  (let (candidate-feats
	eco-feats
	training-set
	holding-set
	dataset
	split-index
	images
	category-index-pairs)

    ;; Read in the data, create training and test set
    (multiple-value-bind (images category-index-pairs) (read-images dirs)
      (setq dataset (random-shuffle images))
      (setq split-index (round (* .7 (length dataset))))
      (setq training-set (subseq dataset 0 split-index))
      (setq holding-set (subseq dataset split-index))
      (format t "Creating Solutions ...~%")
      (loop
	 for i from 0 to (- size-pop 1)
	 do
	   (push (create-creature) candidate-feats))
      (format t "Creatures:~%~A~%" candidate-feats)
      (loop
	 for i from 0 to (- num-generations 1)
	 do
	   (loop ;; I need to verify if a candidate feature is good
	      for creature in candidate-feats
	      with str-features = nil
	      do
		(setq str-features (make-str-features (getf creature :features)))
		(format t "Training on ~d images~%" (length training-set))
		(loop 
		   for image in training-set
		   with img = nil
		   do
		     (format t "Transforming image...~%")
		     (setq img (pre-process-image str-features img))
		     (format t "Training perceptron...~%")
		     (train (getf creature :perceptron) img))
		(loop
		   for hold in holding-set
		   with fitness = 0 and tp = 0 and tn = 0 and fp = 0 and fn = 0
		   do
		     (setq fitness (fitness creature hold))
		   ;;finally 
		   ;; set creatures fitness
		   ;; if creature's fitness > thresh, then save to eco-feats
		     ))
	   (generate-next-generation)))))

#|
(random-shuffle (read-images '(("/home/david/Code/courses/eecs_741/256_ObjectCategories/002.american-flag/*.*" american-flag) ("/home/david/Code/courses/eecs_741/256_ObjectCategories/003.backpack/*.*" backpack))))
|#
