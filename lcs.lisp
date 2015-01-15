(defun lcs-length (string0 string1)
  (declare (optimize (debug 3)))
  
  (let* ((length0 (length string0))
         (length1 (length string1))
         (tbl (make-array `(,(1+ length0) ,(1+ length1))
                          :element-type 'fixnum
                          :initial-element 0)))
    (loop for i from 1 to length0
       do (loop for j from 1 to length1
             do (if (char= (aref string0 (1- i))
                           (aref string1 (1- j)))
                    (setf (aref tbl i j) (1+ (aref tbl (1- i) (1- j))))
                    (setf (aref tbl i j) (max (aref tbl (1- i) j)
                                              (aref tbl i (1- j)))))))
    tbl))

(defun lcs (string0 string1)
  (declare (optimize (debug 3)))

  (let* ((length0 (length string0))
         (length1 (length string1))
         (tbl (lcs-length string0 string1))
         (s0 (concatenate 'string " " string0))
         (s1 (concatenate 'string " " string1)))
    (print (length s0))
    (print (length s1))
    (labels ((rec-lcs (i j)
               (cond ((or (= 0 i) (= 0 j)) "")
                     ((char= (aref s0 i) (aref s1 j))
                      (concatenate 'string (rec-lcs (1- i) (1- j))
                                   (string  (aref s0 i))))
                     ((> (aref tbl (1- i) (- j 2)) (aref tbl (- i 2) (1- 2)))
                      (rec-lcs i (1- j)))
                     (t
                      (rec-lcs (1- i) j)))))
      (rec-lcs length0 length1))))
