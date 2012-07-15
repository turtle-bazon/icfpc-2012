
(in-package :lambda-lifter)

(defun path-length (path)
  (iter (for step in (funcall path))
        (counting (member step '(:L :R :U :D :W :S)))))

(defun dump-path (stream path)
  (when path
    (iter (with seq = '())
          (for step in (funcall path))
          (when (member step '(:A :L :R :U :D :W :S))
            (push step seq))
          (finally        
           (return (format stream "~{~a~}~%" seq))))))

(defun path-set-cleared (world objects path metadata)
  (values world
          (lambda (type)
            (if (eq type :route-start)
                (list world objects path metadata)
                (funcall objects type)))
          path
          metadata))

