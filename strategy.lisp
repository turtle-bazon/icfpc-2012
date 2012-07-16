
(in-package :lambda-lifter)

(defun object-sq-dist (rx ry ox oy)
  (let ((x-diff (- rx ox))
        (y-diff (- ry oy)))
    (+ (* x-diff x-diff) (* y-diff y-diff))))

(defun will-free-a-rock (ox oy world objects path metadata)
  (let ((object-type (funcall world ox oy))
        (game-turn (make-game-turn :W)))
    (with-robot-coords (rx ry) objects
      (multiple-value-bind (world-orig objects-orig path-orig metadata-orig)
          (funcall game-turn world objects path metadata)
        (declare (ignore world-orig path-orig metadata-orig))
        (multiple-value-bind (world-free objects-free path-free metadata-free)
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
          (declare (ignore world-free path-free metadata-free))
          (not (equal (funcall objects-orig :rock)
                      (funcall objects-free :rock))))))))

(defun make-targets-importancy-comparator (world objects path metadata)
  (with-robot-coords (rx ry) objects
    (let ((portal-coords (first (append (funcall objects :closed-lambda-lift)
                                        (funcall objects :open-lambda-lift))))
          (has-flooding-p (map-has-flooding-p world objects metadata)))
      (with-coords (px py) portal-coords
        (lambda (target-a target-b)
          (with-coords (tax tay) target-a
            (with-coords (tbx tby) target-b
              (flet ((accessible-p (x y) (member (funcall world x y) '(:earth nil))))
                (let ((will-free-rock-a (will-free-a-rock tax tay world objects path metadata))
                      (will-free-rock-b (will-free-a-rock tbx tby world objects path metadata))
                      (robot-dist-a (object-sq-dist rx ry tax tay))
                      (robot-dist-b (object-sq-dist rx ry tbx tby))
                      (portal-dist-a (object-sq-dist px py tax tay))
                      (portal-dist-b (object-sq-dist px py tbx tby))
                      (direct-access-a (a*-search/accessible tax tay rx ry #'accessible-p))
                      (direct-access-b (a*-search/accessible tbx tby rx ry #'accessible-p)))
                  (cond ((and direct-access-a (not direct-access-b)) t)
                        ((and direct-access-b (not direct-access-a)) nil)
                        ((and has-flooding-p (< tay tby)) t)
                        ((and has-flooding-p (> tay tby)) nil)
                        ((and will-free-rock-b (not will-free-rock-a)) t)
                        ((and will-free-rock-a (not will-free-rock-b)) nil)
                        ((and has-flooding-p (> portal-dist-a portal-dist-b)) t)
                        ((and has-flooding-p (> portal-dist-b portal-dist-a)) nil)
                        (t (< robot-dist-a robot-dist-b))))))))))))

(defun find-most-important-object (type world objects path metadata)
  (iter (for coords in (funcall objects type))
        (collect coords into targets-facts)
        (finally
         (when targets-facts
           (return (car (sort targets-facts (make-targets-importancy-comparator world objects path metadata))))))))

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

(deffact not-sinking-yet
  (when (map-has-flooding-p world objects metadata)
    (with-meta-bind (metadata water flooding waterproof)
      (let ((underwater (or (funcall objects :underwater) 0))
            (level (water-level water flooding path)))
        (and (under-water-p objects level)
             (< underwater (truncate waterproof 2)))))))


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
(defmaybe not-sinking-yet fact-not-sinking-yet :predicate)

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
                 maybe-not-sinking-yet
                 maybe-less-lambdas-eaten
                 maybe-moving-action
                 maybe-target-nearer
                 maybe-score-better)))

