
(in-package :lambda-lifter)

(defun path-length (path)
  (iter (for step in (funcall path))
        (counting (member step '(:L :R :U :D :W)))))

(defun visited-p (sample-dx sample-dy path)
  (iter (with dx = 0)
        (with dy = 0)
        (for move in (funcall path))
        (ecase move
          (:L (incf dx))
          (:R (decf dx))
          (:U (decf dy))
          (:D (incf dy))
          (:W (next-iteration))
          (:cleared (leave)))
        (when (and (= dx sample-dx) (= dy sample-dy))
          (return-from visited-p t))))

(defun dump-path (stream path)
  (when path
    (iter (with seq = '())
          (for step in (funcall path))
          (when (member step '(:L :R :U :D :W))
            (push step seq))
          (finally        
           (format stream "~{~a~}" seq)))))

(defun path-set-cleared (world objects path metadata)
  (values world objects (lambda () (cons :cleared (funcall path))) metadata))

