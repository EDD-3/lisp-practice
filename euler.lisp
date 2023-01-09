;;;Practice project euler
(defun get-sum-multiples-of-3-5 (number)
  (let ((x (- number 1)))
    (cond ((< x 3) 0)
          ((or (zerop (mod x 3)) (zerop (mod x 5))) (+ x (get-sum-multiples-of-3-5 x)))
          (t (get-sum-multiples-of-3-5 x)))))

(defun fibonacci (n)
  (cond ((< n 2) 1)
        ( t (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
