
(in-package :lambda-lifter)

(defun estimate-importance (rx ry ox oy width height)
  (+ (* oy (+ (* width width) (* height height)))
     (let ((x-diff (- rx ox))
           (y-diff (- ry oy)))
       (+ (* x-diff x-diff) (* y-diff y-diff)))))

(defun find-most-important-object (type world objects path metadata)
  (declare (ignore world path))
  (with-robot-coords (rx ry) objects
    (with-meta-bind (metadata width height)
      (iter (for coords in (funcall objects type))
            (with-coords (ox oy) coords
              (finding coords minimizing (estimate-importance rx ry ox oy width height)))))))

(defun choose-target (world objects path metadata)
  (iter (for possible-targets in '(:lambda :open-lambda-lift))
        (for nearest-object = (find-most-important-object possible-targets world objects path metadata))
        (when nearest-object
          (return-from choose-target nearest-object))))

(defun make-estimator (target move score world objects path metadata)
  (with-coords (target-x target-y) target
    (lambda (proc) (funcall proc target-x target-y move score world objects path metadata))))

;; world position estimating

(defmacro deffact (name &body body)
  `(defun ,(form-symbol 'fact- name) (estimator)
     (funcall estimator
              (lambda (target-x target-y move score world objects path metadata)
                (declare (ignorable target-x target-y move score world objects path metadata))
                ,@body))))

(deffact sq-dist
  (with-robot-coords (rx ry) objects
    (let ((x-diff (- target-x rx))
          (y-diff (- target-y ry)))
      (+ (* x-diff x-diff) (* y-diff y-diff)))))

(deffact lambdas-eaten
  (length (funcall objects :collected-lambda)))

(deffact target-reached
  (with-robot-coords (rx ry) objects
    (and (= rx target-x) (= ry target-y))))

(deffact moving-action
  (when (member move '(:L :R :U :D)) t))

(deffact score score)


(defmacro defmaybe (name fact check-type)
  `(defun ,(form-symbol 'maybe- name) (estimator-a estimator-b k)
     (let ((fact-a (,fact estimator-a))
           (fact-b (,fact estimator-b)))
       (cond ,@(ecase check-type
                      (:less
                       `(((< fact-a fact-b) t)
                         ((> fact-a fact-b) nil)
                         (t (funcall k))))
                      (:more
                       `(((> fact-a fact-b) t)
                         ((< fact-a fact-b) nil)
                         (t (funcall k))))
                      (:predicate
                       `(((and fact-a (not fact-b)) t)
                         ((and fact-b (not fact-a)) nil)
                         (t (funcall k)))))))))

(defmaybe target-nearer fact-sq-dist :less)
(defmaybe target-reached fact-target-reached :predicate)
(defmaybe less-lambdas-eaten fact-lambdas-eaten :less)
(defmaybe moving-action fact-moving-action :predicate)
(defmaybe score-better fact-score :more)

(defmacro check-facts ((target position-a position-b) &rest clauses)
  (with-gensyms (ea eb)
    (labels ((builder (maybes)
               (if maybes
                   `(,(car maybes) ,ea ,eb (lambda () ,(builder (cdr maybes))))
                   't)))
      `(let ((,ea (apply #'make-estimator ,target ,position-a))
             (,eb (apply #'make-estimator ,target ,position-b)))
         ,(builder clauses)))))
                          
(defun make-positions-comparator (target)
  (lambda (position-a position-b)
    (check-facts (target position-a position-b)
                 maybe-target-reached
                 maybe-less-lambdas-eaten
                 maybe-moving-action
                 maybe-target-nearer
                 maybe-score-better)))

