
(in-package :lambda-lifter)

(defun target-accessible-p (target-x target-y world metadata)
  (labels ((examine-accessible (x y)
             (case (funcall world x y)
               ((:robot :target-1 :target-2 :target-3 :target-4 :target-5 :target-6 :target-7 :target-8 :target-9)
                (return-from target-accessible-p t))
               ((:wall :rock :lambda :closed-lambda-lift :open-lambda-lift
                       :portal-a :portal-b :portal-c :portal-d :portal-e :portal-f :portal-g :portal-h :portal-i
                       :beard :razor)
                nil)
               (t t)))
           (examine-around (x y history)
             (iter (for (dx dy) in (list (list (1- x) y)
                                         (list (1+ x) y)
                                         (list x (1+ y))
                                         (list x (1- y))))
                   (when (and (in-range-p metadata dx dy)
                              (examine-accessible dx dy)
                              (not (position (complex dx dy) history)))
                     (collect (list dx dy) into accessible-list))
                   (finally
                    (iter (for (dx dy) in accessible-list)
                          (examine-around dx dy (cons (complex x y) history)))))))
    (examine-around target-x target-y (list (complex target-x target-y)))))

(defun object-sq-dist (rx ry ox oy)
  (let ((x-diff (- rx ox))
        (y-diff (- ry oy)))
    (+ (* x-diff x-diff) (* y-diff y-diff))))

(defun will-free-a-rock (ox oy world objects path metadata)
  (let ((object-type (funcall world ox oy)))
    (with-robot-coords (rx ry) objects
      (multiple-value-bind (world objects path metadata)
          (funcall (make-game-turn :W)
                   (lambda (x y)
                     (cond ((and (= x ox) (= y oy)) nil)
                           ((and (= x rx) (= y ry)) nil)
                           (t (funcall world x y))))
                   (lambda (type)
                     (cond ((eq type object-type) (remove (complex ox oy) (funcall objects type)))
                           (t (funcall objects type))))
                   path
                   metadata)
        (declare (ignore objects path metadata))
        (eq (funcall world ox oy) :rock)))))

(defun make-targets-importancy-comparator (world objects path metadata)
  (with-robot-coords (rx ry) objects
    (let ((portal-coords (first (append (funcall objects :closed-lambda-lift)
                                        (funcall objects :open-lambda-lift))))
          (has-flooding-p (map-has-flooding-p world objects metadata)))
      (with-coords (px py) portal-coords
        (lambda (target-a target-b)
          (with-coords (tax tay) target-a
            (with-coords (tbx tby) target-b
              (let ((will-free-rock-a (will-free-a-rock tax tay world objects path metadata))
                    (will-free-rock-b (will-free-a-rock tbx tby world objects path metadata))
                    (robot-dist-a (object-sq-dist rx ry tax tay))
                    (robot-dist-b (object-sq-dist rx ry tbx tby))
                    (portal-dist-a (object-sq-dist px py tax tay))
                    (portal-dist-b (object-sq-dist px py tbx tby)))
                (cond ((and will-free-rock-b (not will-free-rock-a)) t)
                      ((and will-free-rock-a (not will-free-rock-b)) nil)
                      ((and has-flooding-p
                            (> portal-dist-a portal-dist-b)) t)
                      ((and has-flooding-p
                            (> portal-dist-b portal-dist-a)) nil)
                      (t (< robot-dist-a robot-dist-b)))))))))))

(defun find-most-important-object (type world objects path metadata)
  (iter (for coords in (funcall objects type))
        (with-coords (ox oy) coords
          (when (target-accessible-p ox oy world metadata)
            (collect coords into targets-facts)))
        (finally
         (return (car (sort targets-facts (make-targets-importancy-comparator world objects path metadata)))))))

(defun choose-target (world objects path metadata)
  (iter (for possible-targets in '(:lambda :open-lambda-lift :portal-a :portal-b :portal-c :portal-d :portal-e :portal-f :portal-g :portal-h :portal-i :razor))
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
                   'nil)))
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

