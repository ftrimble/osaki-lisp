(defun member (lst elem)
  (memberHelper lst elem nil)
)
(defun memberHelper (lst elem best)
  (cond ((endp lst) (and (not (null best)) (= elem best)))
        ((<= elem (car lst)) (memberHelper (cadr lst) elem (car lst)))
        (t (memberHelper (caddr lst) elem best))
  )
)

(member '(2 (1) (3)) 1) ; should be t
