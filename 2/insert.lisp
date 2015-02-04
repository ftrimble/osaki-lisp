(defun createSet (head leftSubtree rightSubtree previous)
  (cond ((and (null leftSubtree) (null rightSubtree)) (list head))
        ((null leftSubtree) (list head rightSubtree))
        ((null rightSubtree) (list head leftSubtree))
        (t (list head leftSubtree rightSubtree))
  )
)

(defun insert (lst elem)
  (insertHelper lst elem nil)
)

(defun insertHelper (lst elem best)
  (cond ((null lst) (if (or (null best) (not (= best elem)))
                        (list elem)
                      nil))
        ((<= elem (car lst)) (createSet (car lst)
                                        (insertHelper (cadr lst) elem (car lst))
                                        (caddr lst)
                                        lst))
        (t (createSet (car lst)
                      (cadr lst)
                      (insertHelper (caddr lst) elem best)
                      lst))
  )
)

;; should be (2 (4))
(insert '(2) 4)

;; should be (2 (1) (3 (4)))
(insert '(2 (1) (3)) 4)

;; should be (2 (1) (3))
(insert '(2 (1) (3)) 2)


;; should be (4 (2) (8 (6))))
(insert '(4 (2) (8)) 6)

;; should be (4 (2 (3)) (8))
(insert '(4 (2) (8)) 3)

;; should be (4 (2 (1)) (8))
(insert '(4 (2) (8)) 1)
