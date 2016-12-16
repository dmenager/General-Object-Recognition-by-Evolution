(load "perceptron")
(load "ada")

(defun standard-dev (l mean)
  (sqrt (* (/ 1.0d0 (length l))
         (reduce #'+ l
             :key (lambda (x)
                (expt (- x mean) 2))))))

#| Create data output file for genetic algorithm |#

;; filename = name of file to create
;; index = index column
;; avg-fitness = average fitness column
;; avg-precision = precision column
;; avg-recall = recall column
;; avg-accuracy = accuracy column
(defun create-file (filename index avg-fitness err-fitness avg-precision err-precision avg-recall err-recall avg-accuracy err-accuracy)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "~S,~S,~S,~S,~S,~S,~S,~S,~S~%" index avg-fitness err-fitness avg-precision err-precision avg-recall err-recall avg-accuracy err-accuracy)))

#| Write data output file for genetic algorithm |#

;; filename = name of file to create
;; index = data for index column
;; avg-fitness = data for average fitness column
;; avg-precision = data for precision column
;; avg-recall = data for recall column
;; avg-accuracy = data for accuracy column
(defun write-to-file (filename index avg-fitness err-fitness avg-precision err-precision avg-recall err-recall avg-accuracy err-accuracy)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :append
			  :if-does-not-exist :error)
    (format stream "~d,~d,~d,~d,~d,~d,~d,~d,~d~%" index avg-fitness err-fitness avg-precision err-precision avg-recall err-recall avg-accuracy err-accuracy)))

#| Save evolved features to a file |#

;; eco-feats = evolved ECO-Features
(defun save-features (eco-feats)
  (with-open-file (stream "eco-features.txt"
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (mapcar #'(lambda (feat)
		(format stream "~S~%" feat))
	    eco-feats)))

(defun load-features ()
  (let (features)
    (with-open-file (stream "eco-features.txt"
			    :direction :input
			    :if-does-not-exist :error)
      (do ((feature (read stream nil nil) (read stream nil nil))
	   (i 0 (incf i)))
	  ((null feature)
	   (format t "Finished loading features~%"))
	(format t "Adding feature ~d~%" i)
	
        (setq features (cons (list :features (second feature) :perceptron (fourth feature)) features))))
    features))

#| Shuffle list |#

;; sequence = dataset
(defun random-shuffle (sequence)
  (map-into sequence #'car
            (sort (map 'vector (lambda (x)
                                 (cons x (random 1d0)))
                       sequence)
                  #'< :key #'cdr)))

#| Define a random number in range [start, end] |#

;; start = start of range
;; end = end of range
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

#| Close the pipe, close all streams to the process|#

;; process = process started by sb-ext
(defun kill-and-close-process (process)
  (sb-ext:process-kill process 15 :pid)
  (sb-ext:process-wait process)
  (sb-ext:process-close process)
  (sb-ext:process-exit-code process))

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
    (setq out (substitute #\: #\' out))
    (setq out (substitute #\Space #\: out))
    (kill-and-close-process p)
    
    (with-input-from-string (s out)
      (do ((form (read s nil nil) (read s nil nil)))
	  ((null form)
	   (setq res (reverse res)))
	(push form res)))
    res))

(defun create-transforms (transforms)
  (let (num-transforms)
    (if (= 1 (length transforms))
	(setq num-transforms 1)
	(setq num-transforms (+ 1 (random (- (length transforms) 1)))))
    (format t "Selected number of transforms: ~d~%" num-transforms)
    (loop
       for i from 1 to num-transforms
       with transform-template = nil and transform-types = nil and transform-name = nil
       do
	 (setq transform-template (nth (random (length transforms)) transforms))
	 (setq transform-types (getf transform-template :types))
	 (setq transform-name (getf transform-template :name))
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
(defun create-creature (max-child-size transforms &optional (enable-roi t))
  (let (max-width max-height x1 x2 y1 y2 perceptron feats region)
    (setq max-width 1000)
    (setq max-height 1000)

    (format t "Creating Transformations.~%")
    (setq feats (create-transforms transforms))
    (when (<= (length feats) max-child-size)
      (format t "Defining ROI.~%")
      (setq x1 (rand-in-range 0 (- max-width 2)))
      (setq x2 (rand-in-range (+ 1 x1) (- max-width 1)))
      (setq y1 (rand-in-range 0 (- max-height 2)))
      (setq y2 (rand-in-range (+ 1 y1) (- max-height 1)))
      (setq region (list x1 x2 y1 y2))
      (format t "Creating perceptron.~%")
      (setq perceptron (make-perceptron))
      (format t "Initializing weights.~%")
      (if  enable-roi
	   (initialize-perceptron (* (- x2 x1) (- y2 y1)) perceptron)
	   (initialize-perceptron (* max-width max-height) perceptron))    
      (if enable-roi
	  (list ':features (cons region feats) ':perceptron perceptron ':fitness 0)
	  (list ':features feats ':perceptron perceptron ':fitness 0)))))

#| Create a new child through cross-over |#

;; c1 = creature 1
;; c2 = creature 2
(defun cross-over (c1 c2 max-child-size &optional (enable-roi t))
  (let (max-width max-height picker first-half second-half perceptron x1 x2 y1 y2 region)
    (setq picker (random 2))
    (setq max-width 1000)
    (setq max-height 1000)
    (format t "Genetic Recombination.~%")
    (cond ((= 0 picker)
	   (setq picker (random (length (getf c1 :features))))
	   (setq first-half (subseq (getf c1 :features) 0 picker))
	   
	   (setq picker (random (length (getf c2 :features))))
	   (setq second-half (subseq (getf c2 :features) picker)))
	  ((= 1 picker)
	   (setq picker (random (length (getf c2 :features))))
	   (setq first-half (subseq (getf c2 :features) 0 picker))
	   (setq picker (random (length (getf c1 :features))))
	   (setq second-half (subseq (getf c1 :features) picker))))

    ;; children are capped to max child size
    (when (and (< 0 (length first-half) (length second-half))
	       (<= (+ (length first-half) (length second-half)) max-child-size))
      (format t "Defining ROI.~%")
      (setq x1 (rand-in-range 0 (- max-width 2)))
      (setq x2 (rand-in-range (+ 1 x1) (- max-width 1)))
      (setq y1 (rand-in-range 0 (- max-height 2)))
      (setq y2 (rand-in-range (+ 1 y1) (- max-height 1)))
      (setq region (list x1 x2 y1 y2))
      (format t "Creating perceptron.~%")
      (setq perceptron (make-perceptron))
      (format t "Initializing weights.~%")
      (if enable-roi
	  (initialize-perceptron (* (- x2 x1) (- y2 y1)) perceptron)
	  (initialize-perceptron (* max-width max-height) perceptron))
      (list ':features (append first-half second-half) ':perceptron perceptron ':fitness 0))))

#| Mutate child |#

;; c1 = child
;; mutation-prob = probability of mutating child
;; transforms = transformation templates
(defun mutate (c1 mutation-prob transforms)
  (when (not (null c1))
    (format t "Original Child features: ~A~%" (getf c1 :features))
    (let (chance)
      (setq chance (random 101))
      (cond ((<= chance (* 100 mutation-prob))
	     (format t"Mutating child.~%")
	     (let (mutate-index feature feature-mutate-index mutated-feature)
	       ;; pick place to change
	       (setq mutate-index (random (length (getf c1 :features))))
	       (setq feature (nth mutate-index (getf c1 :features)))
	       
	       ;; pick position to change
	       (setq feature-mutate-index (random (length feature)))
	       (cond ((= 0 feature-mutate-index)
		      ;; get transform
		      (let (new-transforms)
			(setq new-transforms (list (nth (random (length transforms)) transforms)))
			(setq mutated-feature (car (create-transforms new-transforms)))))
		     (t
		      ;; find transform and get a new value for it
		      (let (template)
			(loop
			   named filter
			   for feature-template in transforms
			   do
			     (when (eq (getf feature-template :name) (first feature))
			       (setq template feature-template)
			       (return-from filter)))
			(loop
			   for i from 1 to (- (length feature) 1)
			   ;; keep everything else the same
			   if (not (= i feature-mutate-index))
			   collect (nth i feature) into accumulator
			   else
			   collect
			     (let (type type-val)
			       ;; get the type of the change
			       (setq type (nth i (getf template :types)))
			       (cond
				 ((equal 'img type))
				 ((equal 'float type)
				  (setq type-val (random 100.00)))
				 ((equal 'int type)
				  (setq type-val (random 100)))
				 ((listp type)
				  (setq type-val (nth (random (length type)) type)))
				 ((equal 'bool type)
				  (setq type-val (nth (random 2) '(t nil))))
				 (t (error "Unsupported transform type: ~A" type)))
			       type-val) into accumulator
			   finally
			     (setq mutated-feature (cons (car feature) accumulator))))))
	       ;; change
	       (setf (nth mutate-index (getf c1 :features)) mutated-feature)
	       (format t "Child mutated to:~A~%~%" (getf c1 :features))
	       c1))
	    (t
	     (format t "No mutations performed. Returning Child.~%~%")
	     c1)))))

#| Create the next generation of creatures|#

;; candidate-feats = current generation
;; mutation-prob = mutation probability of child
;; transforms = transformations 
(defun generate-next-generation (candidate-feats mutation-prob max-child-size transforms generation-num)
  (let (fitnesses mean-fitness most-fit next-gen min-pop)
    (setq fitnesses (mapcar #'(lambda (feat)
				(getf feat :fitness))
			    candidate-feats))
    (setq mean-fitness (/ (reduce '+ fitnesses) (length fitnesses)))
    (when  (> (length fitnesses) 0 )
      (setq most-fit
	    (mapcan #'(lambda (creature)
			(when (>= (getf creature :fitness) mean-fitness)
			  (list creature)))
		    candidate-feats))
      (format t "Average Fitness of Generation ~d: ~d~%" generation-num mean-fitness)
      (setq min-pop 5)
      (loop
	 named business-time
	 with stop-at-min = nil
	 while (< (length next-gen) min-pop)
	 do
	   (loop
	      named inner
	      for creature1 in most-fit
	      do
		(loop
		   for creature2 in most-fit
		   with child = nil
		   when (not (equal creature1 creature2))
		   do
		     (setq child (mutate (cross-over creature1 creature2 max-child-size nil) mutation-prob transforms))
		     (when (not (null child))
		       (setq next-gen (cons child next-gen)))
		     (when (and stop-at-min
				(= (length next-gen)
				   min-pop))
		       (return-from inner))))
	   (when (and (not stop-at-min)
		      (< (length next-gen) min-pop))
	     (setq stop-at-min t))
	 finally
	   (format t "Next generation contains ~d members~%"(length next-gen))
	   (return-from business-time next-gen)))))

#| Run the AdaBoost classifier|#

;; eco-feats = evolutionarily constructed features
;; training-set = training set for classification trainig
;; holding-set = validation
;; category-index-pairs =  mapping from image-label to number
(defun ada-boost (eco-feats training-set holding-set category-index-pairs)
  (let (boost max-classifiers)
	(setq max-classifiers 100)
	(create-file "boost.csv" 'Tau 'Nothing 'Nothing 'Precision 'nothing 'Recall 'nothing 'Accuracy 'nothing)
	(loop
	   for tau from 0 to (min max-classifiers (length eco-feats))
	   do
	     (format t "Initializing Adaboost classifier from ~d ECO-Features~%" (length eco-feats))
	     (setq boost (initialize-ada-boost max-classifiers eco-feats))
	     
	     (format t "Adaboost weights before training.~%~A~%" (ada-boost-classifier-weights boost))
	     (train-ada-boost boost training-set tau)
	     (format t "Adaboost weights after training.~%~A~%" (ada-boost-classifier-weights boost))

	     (format t "------------------------------------~%")
	     (format t "Testing Adaboost on ~d images~%" (length holding-set))
	     (format t "------------------------------------~%")
	     (loop
		for hold in holding-set
		with tp = 0 and tn = 0 and fp = 0 and fn = 0
		do
		  (let (predicted ground hold-label)
		    (setq hold-label (getf hold :label))
		    (format t "Predicting with Adaboost classifier~%")
		    (setq predicted (ada-classify boost hold tau))
		    (setq ground (car (assoc hold-label category-index-pairs)))
		    (cond ((and (= predicted 0) (= ground 0))
			   (incf tn))
			  ((and (= predicted 1) (= ground 1))
			   (incf tp))
			  ((and (= predicted 0) (= ground 1))
			   (incf fn))
			  ((and (= predicted 1) (= ground 0))
			   (incf fp))))
		finally
		  (write-to-file "boost.csv" tau 0 0 (/ tp (+ tp fp)) 0 (/ tp (+ fp tp)) 0 (/ (+ tp tn) (+ tp tn fp fn)) 0)
		  (format t "~%ADA BOOST CONFUSION MATRIX:~%~A~%~%"
			  (list (list tp fp) (list fn tn)))))))

#| Genetic algorithm to evolve eco-features |#

;; size-pop = size of population
;; num-generations = number of generations to create
;; category-index-pairs = mapping from image-label to number
;; training-set = training set for ECO features
;; holding-set = validation
(defun evolve-features (size-pop num-generations category-index-pairs training-set holding-set)
  (let (candidate-feats
	eco-feats
	transforms
	learning-rate
	bias
	max-child-size)
    ;; leaving out optional params
    ;; scikit-image transforms
    (setq bias 0)
    (setq learning-rate 1)
    (setq max-child-size 5)
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
		;;'(:name threshold-adaptive :types (img int ("generic" "gaussian" "mean" "median")))
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
    (format t "Creating Solutions ...~%")
    (loop
       while (< (length candidate-feats) size-pop)
       with creature = nil
       do
	 (setq creature (create-creature max-child-size transforms nil))
	 (cond ((not (null creature))
		(format t "Creature SUCCESS~%~%")
		(push creature candidate-feats))
	       (t
		(format t "Creature FAILED~%~%"))))
    (create-file "generations.csv"
		 'Generations
		 'Average-Fitness
		 'Fitness-Error
		 'Average-Precision
		 'Precision-Error
		 'Average-Recall
		 'Recall-Error
		 'Average-Accuracy
		 'Accuracy-Error)
    (loop
       for i from 0 to (- num-generations 1)
       with avg-fitness = 0 and avg-precision = 0 and avg-recall = 0 and avg-accuracy = 0
       with err-fitness = 0 and err-precision = 0 and err-recall = 0 and err-accuracy = 0
       with fs = nil
       with precisions = nil and recalls = nil and accuracys = nil
       do
	 (format t "GENERATION: ~d~%" i)
	 (loop
	    for creature in candidate-feats
	    with str-features = nil
	    do
	      (block continue
		(setq str-features (make-str-features (getf creature :features)))
		(format t "------------------------------------~%")
		(format t "Training on ~d images~%" (length training-set))
		(format t "------------------------------------~%")
		(loop 
		   for image in training-set
		   with img = nil and img-weights
		   do
		     (format t "Transforming ~A~%Image Class: ~d~%With ~%~A~%"
			     (getf image :image)
			     (car (assoc (getf image :label) category-index-pairs))
			     str-features)
		     (setq img (pre-process-image str-features (getf image :image)))
		     
		   ;; print any errors from python
		     (when (not (numberp (first img)))
		       (format t "~A~%~%" img)
		       (setq candidate-feats
			     (remove creature candidate-feats
				     :key #'(lambda (x) (getf x :features))
				     :test 'equal))
		       (return-from continue))
		     
		     (format t "Training perceptron..~%")
		     (setq img-weights
			   (train (getf creature :perceptron)
				  (list ':image img ':label (getf image ':label))
				  learning-rate
				  bias))
		     (setf (perceptron-weights (getf creature :perceptron))
			   (set-weights img-weights
					(subseq (perceptron-weights (getf creature :perceptron))
						(length img-weights)))))
		(format t "------------------------------------~%")
		(format t "Testing on holdout set of size ~d~%" (length holding-set))
		(format t "------------------------------------~%")
		(loop
		   for hold in holding-set
		   with tp = 0 and tn = 0 and fp = 0 and fn = 0
		   with accuracy = 0 and precision = 0 and recall = 0
		   with precision-defined = t and recall-defined = t
		   with img = nil
		   with err-pen = 5 and reg-pen = 1 and alpha = .4
		   do
		     (setq img (pre-process-image str-features (getf hold :image)))
		     (let (predicted ground hold-label)
		       (setq hold-label (getf hold :label))
		       (setq predicted (classify img (getf creature :perceptron) 1))
		       (setq ground (car (assoc hold-label category-index-pairs)))
		       (format t "Transforming ~A~%Image Class: ~d~%With ~%~A~%...~%"
			       (getf hold :image)
			       ground
			       str-features)
		       (cond ((and (= predicted 0) (= ground 0))
			      (setq tn (+ tn 1)))
			     ((and (= predicted 1) (= ground 1))
			      (setq tp (+ tp 1)))
			     ((and (= predicted 0) (= ground 1))
			      (setq fn (+ fn 1)))
			     ((and (= predicted 1) (= ground 0))
			      (setq fp (+ fp 1)))))
		   finally
		     (let (ordinary-fitness-score)
		       (setq ordinary-fitness-score (calculate-fitness tp tn fp fn err-pen alpha))
		       (setf (getf creature :fitness) (- ordinary-fitness-score
							 (* (length (getf creature :features))
							    reg-pen)))
		       (format t "Ordinary Fitness score for ~A~%~d~%" str-features ordinary-fitness-score)
		       (format t "Regularized Fitness score for ~A~%~d~%" str-features (getf creature :fitness))
		       (push (getf creature :fitness) fs)
		       
		       (cond ((= 0 tp fp)
			      (setq precision-defined nil)
			      (setq precision 0))
			     ((= 0 tp fn)
			      (setq recall-defined nil)
			      (setq recall 0)))
		       (setq accuracy (/ (+ tp tn) (+ tp tn fp fn)))
		       (when recall-defined
			 (setq recall (/ tp (+ fn tp))))
		       (when precision-defined
			 (setq precision (/ tp (+ tp fp))))
		       (format t "True Positives: ~d True Negatives: ~d False Positives: ~d False Negatives: ~d~%" tp tn fp fn)
		       (format t "Accuracy: ~d Precision: ~d Recall: ~d~%~%" accuracy precision recall)
		       (push accuracy accuracys)
		       (push precision precisions)
		       (push recall recalls)
		       (when (> (getf creature :fitness) 450)
			 (format t "Added creature to ECO Features!~%")
			 (push creature eco-feats))))))
	 
	 (setq avg-fitness (/ (reduce '+ fs) (length fs)))
	 (setq avg-precision (/ (reduce '+ precisions) (length precisions)))
	 (setq avg-recall (/ (reduce '+ recalls) (length recalls)))
	 (setq avg-accuracy (/ (reduce '+ accuracys) (length accuracys)))
	 
	 (setq err-fitness (standard-dev fs avg-fitness))
	 (setq err-precision (standard-dev precisions avg-precision))
	 (setq err-recall (standard-dev recalls avg-recall))
	 (setq err-accuracy (standard-dev accuracys avg-accuracy))
	 (write-to-file "generations.csv" i
			avg-fitness err-fitness
			avg-precision err-precision
			avg-recall err-recall
			avg-accuracy err-accuracy)
	 (setq candidate-feats (generate-next-generation candidate-feats .35 max-child-size transforms i)))
    (save-features eco-feats)
    eco-feats))

(defun run (loadp)
  (let (eco-features
	training-set
	holding-set
	dataset
	split-index)
    ;; Read in the data, create training and test set
    (multiple-value-bind (images category-index-pairs)
	(read-images '(("/home/david/Code/courses/eecs_741/256_ObjectCategories/002.american-flag/*.*" american-flag)
		       ("/home/david/Code/courses/eecs_741/256_ObjectCategories/003.backpack/*.*" backpack)))
      (setq dataset (random-shuffle images))
      (setq split-index (round (* .7 (length dataset))))
      (setq training-set (subseq dataset 0 split-index))
      (setq holding-set (subseq dataset split-index))

      (cond (loadp
	     (setq eco-features (load-features)))
	    (t
	     (setq eco-features
		   (evolve-features 5 5  category-index-pairs training-set holding-set))))
      (ada-boost eco-features training-set holding-set category-index-pairs))))

#|
(random-shuffle (read-images '(("/home/david/Code/courses/eecs_741/256_ObjectCategories/085.goat/*.*" goat) ("/home/david/Code/courses/eecs_741/256_ObjectCategories/084.giraffe/*.*" giraffe))))

(evolve-features 5 5 '(("/home/david/Code/courses/eecs_741/256_ObjectCategories/002.american-flag/*.*" american-flag) ("/home/david/Code/courses/eecs_741/256_ObjectCategories/003.backpack/*.*" backpack)))

;; TEST CROSS-OVER
(cross-over (list ':features '((GABOR 25.139236 2.700901 1.0129333 29.400885 41 34.586823 reflect 1)(INTEGRAL-BLUR)(RANK-ORDER)(EQUALIZE-HIST 61)(HISTOGRAM 71)(GRADIENT 31)(HISTOGRAM 62)(HISTOGRAM 60)(RANK-ORDER)(EQUALIZE-HIST 59)(SOBEL)(DILATION 22)(DILATION 45)(RANK-ORDER)) ':perceptron nil)
	    (list ':features '((EQUALIZE-HIST 13)(RANK-ORDER)(EROSION 63)(HOUGH-CIRCLE 86.685455 T)) ':perceptron nil))

;; TEST MUTATION
(mutate (cross-over (list ':features '((GABOR 25.139236 2.700901 1.0129333 29.400885 41 34.586823 reflect 1)(INTEGRAL-BLUR)(RANK-ORDER)(EQUALIZE-HIST 61)(HISTOGRAM 71)(GRADIENT 31)(HISTOGRAM 62)(HISTOGRAM 60)(RANK-ORDER)(EQUALIZE-HIST 59)(SOBEL)(DILATION 22)(DILATION 45)(RANK-ORDER)) ':perceptron nil)
	    (list ':features '((EQUALIZE-HIST 13)(RANK-ORDER)(EROSION 63)(HOUGH-CIRCLE 86.685455 T)) ':perceptron nil)) 1 
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
		  ;;'(:name threshold-adaptive :types (img int ("generic" "gaussian" "mean" "median")))
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

;; Read error 
(pre-process-image "[[DILATION, 29],[MEDIAN-BLUR, 22],[INTEGRAL-BLUR],[GRADIENT, 65],[INTEGRAL-BLUR],[EQUALIZE-HIST, 81],]" #P"/home/david/Code/courses/eecs_741/256_ObjectCategories/003.backpack/003_0026.jpg")
|#
