(defstruct ada-boost classifier-weights classifiers)

#| Load Adaboost classifier |#

(defun load-ada-classifier()
  (with-open-file (stream "classifier.txt"
			  :direction :input
			  :if-does-not-exist :error)
    (let (boost)
      (setq boost (make-ada-boost))
      (setf (ada-boost-classifier-weights boost) (read stream))
      (setf (ada-boost-classifiers boost) (read stream))
      boost)))

#| Save Adaboost model |#

;; boost = Adaboost model
(defun save-ada-classifier (boost)
  (with-open-file (stream "classifier.txt"
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (format stream "~A~%" (ada-boost-classifier-weights boost))
    (format stream "~A~%" (ada-boost-classifiers boost))))

#| Classify an image with Adaboost |#

;; boost = adaboost classifier
;; image = image to classify
;; tau = threshold
(defun ada-classify (boost image tau)
  (let (img str-features)
    (if (> (reduce '+ (mapcar #'(lambda (weight classifier)
			   (setq str-features (make-str-features (getf classifier :features)))
			   (setq img (pre-process-image str-features (getf image :image)))
			   (format t "Transforming ~A with ~%~A~%" (getf image :image) str-features)
			   (format t "Ground: ~d~%" (getf image :label))
			   (* (classify img (getf classifier :perceptron) 1 *standard-output*) weight))
		       (ada-boost-classifier-weights boost)
		       (ada-boost-classifiers boost))) tau)
	1
	0)))

#| Calculate weight for each classifier |#

;; err = classifier error
(defun calculate-classifier-weight (err)
  (* (/ 1 2) (log (/ (- 1 err) err))))

#| Create Adaboost classifier |#

;; max-classifiers = max number of classifiers in model
;; classifiers = extracted ECO Features
(defun initialize-ada-boost (max-classifiers classifiers)
  (let (boost)
    (setq boost (make-ada-boost))
    (if (> (length classifiers) max-classifiers)
	(setf (ada-boost-classifiers boost)
	      (subseq classifiers 0 max-classifiers))
	(setf (ada-boost-classifiers boost) classifiers))
    (setf (ada-boost-classifier-weights boost)
	  (make-list (length (ada-boost-classifiers boost)) :initial-element 0))
    boost))

#| Train the Adaboost classifier |#

;; boost = adaboost classifier
;; training = set of training images
;; tau = threshold for classification
(defun train-ada-boost (boost training tau)
  (let (image-errors perceptron-errors)
    (setq image-errors (make-list (length training) :initial-element (/ 1 (length training))))
    (setq perceptron-errors (make-list (length (ada-boost-classifiers boost)) :initial-element 0))
    (loop
       named outer
       for x from 0 to (- (length (ada-boost-classifiers boost)) 1)
       do
	 (loop
	    for classifier in (ada-boost-classifiers boost)
	    for i from 0 to (- (length perceptron-errors) 1)
	    do
	      (loop
		 for m from 0 to (- (length training) 1)
		 with image = nil and image-class = nil
		 do
		   (setq image (nth m training))
		   (setq image-class (getf image :label))
		   (when (not (= image-class (ada-classify boost image tau)))
		     (setf (nth i perceptron-errors) (+ (nth i perceptron-errors) (nth m image-errors))))))
	 (let (min-perceptron-err coeff)
	   (setq min-perceptron-err (apply 'min perceptron-errors))

	   (when (>=  min-perceptron-err .5)
	     (format t "Bad perceptrons. Minimum Error too high, so training failed, but continuing anyway.~%")
	     (return-from outer))

	   (setq coeff (calculate-classifier-weight min-perceptron-err))
	   (setf (nth (position min-perceptron-err perceptron-errors)
		      (ada-boost-classifier-weights boost))
		 coeff)
	   (loop
	      for m from 0 to (- (length training) 1)
	      with image = nil and image-class = nil
	      with c = 0 and normalization-factor = 0 and numerator = 0
	      do
		(setq image (nth m training))
		(setq image-class (getf image :label))
		(if (= image-class (ada-classify boost image tau))
		    (setq c 1)
		    (setq c -1))
		(setq numerator (* (nth m image-errors) (exp (* (- 0 coeff) c))))
		(setq normalization-factor (* (length image-errors) numerator))
		(setf (nth m image-errors) (/ numerator normalization-factor)))))
    (save-ada-classifier boost)))
