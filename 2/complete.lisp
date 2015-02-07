(defun createTree (root leftSubtree rightSubtree)
  (cond ((and (null leftSubtree) (null rightSubtree)) (list root))
        ((null rightSubtree) (list root leftSubtree))
        (t (list root leftSubtree rightSubtree))
  )
)
(defun complete (elem depth)
  (if (= 1 depth) (list elem)
    (let ((child (complete elem (1- depth))))
      (createTree elem child child))))

(defun createBalancedTree (elem size)
  (if (= 1 size) (list elem)
    (let ((nodesize ; nodesize = ceiling((size - 1)/2)
           (+ (if (evenp size) 1 0) (/ (1- size) 2))))
      (let ((child (createBalancedTree elem nodesize)))
        (createTree elem child (if (evenp size) (removeLeftDeepest child) child))
        )
      )
    )
  )

(defun removeLeftDeepest (tree)
  (if (null (cadr tree)) nil ; last node; remove it
    (createTree (car tree) (removeLeftDeepest (cadr tree)) (caddr tree))
    )
  )

;; should be (1)
(complete 1 1)

;; should be (1 (1) (1))
(complete 1 2)

;; should be (1 (1 (1) (1)) (1 (1) (1)))
(complete 1 3)

;; should be (1 (1 (1)) (1))
(createBalancedTree 1 4)

;; should be (1 (1 (1) (1)) (1 (1)))
(createBalancedTree 1 6)

;; should be (1 (1 (1 (1)) (1)) (1 (1) (1)))
(createBalancedTree 1 8)

;; should be (1 (1 (1 (1)) (1)) (1 (1 (1)) (1)))
(createBalancedTree 1 9)
