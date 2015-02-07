(defun createTree (root leftSubtree rightSubtree)
  (cond ((and (null leftSubtree) (null rightSubtree)) (list root))
        ((null rightSubtree) (list root leftSubtree))
        (t (list root leftSubtree rightSubtree))
  )
)

(defun insert (lst elem)
  (insertUsingOrdinal lst elem nil #'<=)
)

(defun insertUsingOrdinal (lst elem best ordinal)
  (if (null lst)
      (if (and best (= best elem)) nil
        (list elem))
    (let ((root (car lst)) (left (cadr lst)) (right (caddr lst)))
      (cond ((funcall ordinal elem root)
             (createTree root (insertUsingOrdinal left elem root ordinal) right))
            (t (createTree root left (insertUsingOrdinal right elem best ordinal)))
            )
      )
    )
  )


;; should be (2 () (4))
(insert '(2) 4)

;; should be (2 (1) (3 () (4)))
(insert '(2 (1) (3)) 4)

;; should be (2 (1) (3))
(insert '(2 (1) (3)) 2)


;; should be (4 (2) (8 (6))))
(insert '(4 (2) (8)) 6)

;; should be (4 (2 () (3)) (8))
(insert '(4 (2) (8)) 3)

;; should be (4 (2 (1)) (8))
(insert '(4 (2) (8)) 1)
