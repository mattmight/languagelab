(define (f n)
  (if (= n 0)
      1 
      (* n (f (- n 1)))))

