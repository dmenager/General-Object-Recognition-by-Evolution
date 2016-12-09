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
  (+ start (random (+ 1 (- end start)))))

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
		(write-string "]," string-stream))
	    feats)
    (write-string "]" string-stream)
    (get-output-stream-string string-stream)))

#| Apply transformations to image, as described by eco-feature |#

;; transforms = eco-feature in python string of list
;; img = image to apply eco-features to
(defun pre-process-image (transforms img)
  (let (p out string-stream res)
    (setq string-stream (make-string-output-stream))
    (setq p (sb-ext:run-program "python"
				(list "-W"
				      "ignore"
				      (concatenate 'string
						   (sb-unix::posix-getenv "HOME")
						   "/Code/courses/eecs_741/General-Object-Recognition-by-Evolution/imageTransforms.py")			      
				      (namestring img)
				      transforms)
				:search t
				:wait nil
				:output :stream))
 
    (do ((line (read-line (process-output p) nil nil) (read-line (process-output p) nil nil)))
	((null line)
	 (setq out (get-output-stream-string string-stream)))
      (write-string line string-stream))
    (setq out (substitute #\: #\( out))
    (setq out (substitute #\: #\) out))
    (setq out (substitute #\: #\, out))
    (setq out (substitute #\Space #\: out))
    
    (with-input-from-string (s out)
      (do ((form (read s nil nil) (read s nil nil)))
	  ((null form)
	   (setq res (reverse res)))
	(push form res)))
    res))

#| Determine the fitness of a perceptron |#
(defun fitness (a b))

(defun create-transforms ()
  (let (transforms num-transforms)
    ;; leaving out optional params
    ;; scikit-image transforms
    (setq transforms
	  (list '(:name gabor :types (img float float float float int float ("constant" "nearest" "reflect" "mirror" "wrap") int)) 
		'(:name gradient :types (img int))
		;;'(:name square-root :types (img)) 
		'(:name gaussian :types (img int))
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
    (setq num-transforms (random (length transforms)))
    (format t "Selected number of transforms: ~d~%" num-transforms)
    (loop
       for i from 1 to num-transforms
       with cur-transform = nil and transform-types = nil and transform-name = nil
       do
	 (setq cur-transform (nth (random (length transforms)) transforms))
	 (setq transform-types (getf cur-transform :types))
	 (setq transform-name (getf cur-transform :name))
	 (loop
	    for type in transform-types
	    with type-vals = nil
	    do
	      (cond
		((equal 'img type))
		((equal 'float type)
		 (push (random 100.00) type-vals))
		((equal 'int type)
		 (push (random 100) type-vals))
		((listp type)
		 (push (nth (random (length type)) type) type-vals))
		((equal 'bool type)
		 (push (nth (random 2) '(t nil)) type-vals))
		(t (error "Unsupported transform type: ~A" type)))
	    finally (setq transform-types (reverse type-vals)))
	 (push transform-name transform-types)
       collect transform-types into eco-transforms
       finally (return eco-transforms))))

#| Create an ECO-Feature |#

;; enable-roi = eco-features with transformations on Regions of Interest
(defun create-creature (&optional (enable-roi t))
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
    (if enable-roi
	(list ':features (cons region feats) ':perceptron perceptron :fitness nil)
	(list ':features feats ':perceptron perceptron :fitness nil))))

(defun cross-over (c1 c2))

(defun mutate (c1))

#| Create the next generation of creatures|#

;; candidate-feats = current generation
;; mutation-prob = mutation probability of child
(defun generate-next-generation (candidate-feats mutation-prob)
  (let (fitnesses mean-fitness most-fit)
    (setq fitnesses (mapcar #'(lambda (feat) (getf feat :fitness))))
    (setq mean-fitness (/ (reduce '+ fitnesses) (length fitnesses)))

    (setq most-fit
	  (mapcan (creature)
		  (when (> (getf creature :fitness) mean-fitness)
		    (list creature))))
    ))

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
	   (push (create-creature nil) candidate-feats))
      (format t "~%")
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
		     (format t "Transforming ~A with ~%~A~%...~%" (getf image :image) str-features)
		     (setq img (pre-process-image str-features (getf image :image)))

		     ;; print any errors from python
		     (when (not (numberp (first img)))
		       (format t "~A~%" img))
		     
		     (when (or (numberp (car img)))		       
		       ;;(break "Processed:~%~A~%" img)
		       (format t "Training perceptron...~%")
		       (setf (getf creature :perceptron) (train (getf creature :perceptron)
								'(:image img :label (getf image :label))
								.5))))
		(format t "Testing on holdout set of size ~d~%" (length holding-set))
		(loop
		   for hold in holding-set
		   with fitness = 0 and tp = 0 and tn = 0 and fp = 0 and fn = 0
		   with img = nil
		   do
		     (format t "Transforming ~A with ~%~A~%...~%" (getf hold :image) str-features)
		     (setq img (pre-process-image str-features (getf hold :image)))
		     (let (predicted gound)
		       (setq predicted (classify img (getf creature :perceptron) 1))
		       (setq ground (getf hold :label))
		       (cond ((and (= predicted 0) (= ground 0))
			      (incf tn))
			     ((and (= predicted 1) (= gound 1))
			      (incf tp))
			     ((and (= predicted 0) (= ground 1))
			      (incf fn))
			     ((and (= predicted 1) (= ground 0))
			      (incf fp))))
		   finally 
		     (setf (getf creature :fitness) (calculate-fintess tp tn fp fn))
		     (when (>= (getf creature :fitness) 600)
		       (push creature eco-feats))))
	   (setq candidate-feats (generate-next-generation candidate-feats))))))

#|
(random-shuffle (read-images '(("/home/david/Code/courses/eecs_741/256_ObjectCategories/002.american-flag/*.*" american-flag) ("/home/david/Code/courses/eecs_741/256_ObjectCategories/003.backpack/*.*" backpack))))

(evolve-features 10 10 '(("/home/david/Code/courses/eecs_741/256_ObjectCategories/002.american-flag/*.*" american-flag) ("/home/david/Code/courses/eecs_741/256_ObjectCategories/003.backpack/*.*" backpack)))
|#
