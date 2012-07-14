
(in-package :lambda-lifter)

(defun water-update (world objects path metadata)
  (with-meta-bind (metadata water flooding)
    (if (and water flooding)
        (let* ((water-level (+ water (floor (path-length path) flooding)))
               (ry (imagpart (robot-coords objects))))
          (values world
                  (let ((prev-underwater (funcall objects :underwater)))
                    (if (>= water-level ry)
                        (lambda (type)
                          (case type
                            (:underwater (if prev-underwater
                                             (+ prev-underwater 1)
                                             0))
                            (t (funcall objects type))))
                        (if prev-underwater
                            (lambda (type)
                              (case type
                                (:underwater nil)
                                (t (funcall objects type))))
                            objects)))
                  path
                  metadata))
        (values world objects path metadata))))
