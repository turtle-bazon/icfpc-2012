
(in-package :lambda-lifter)

(defun water-level (water flooding path)
  (+ water (floor (path-length path) flooding)))

(defun under-water-p (objects water-level)
  (with-robot-coords (rx ry) objects
    (declare (ignore rx))
    (>= water-level ry)))

(defun water-update (world objects path metadata)
  (if (map-has-flooding-p world objects metadata)
      (with-meta-bind (metadata water flooding)
        (values world
                (let ((prev-underwater (funcall objects :underwater)))
                  (if (under-water-p objects (water-level water flooding path))
                      (lambda (type)
                        (case type
                          (:underwater (if prev-underwater
                                           (+ prev-underwater 1)
                                           1))
                          (t (funcall objects type))))
                      (if prev-underwater
                          (lambda (type)
                            (case type
                              (:underwater nil)
                              (t (funcall objects type))))
                          objects)))
                path
                metadata))
      (values world objects path metadata)))
