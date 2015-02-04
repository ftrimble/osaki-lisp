(defun complete (elem depth)
  (if (= 1 depth) (list elem)
      (let ((x (complete elem (1- depth))))
        (list elem x x))))

(defun balancedStart (elem size)
  (cond ((= 1 size) (list elem))
        ((= 2 size) (list elem (list elem)))
        (t nil)
  )
)

(defun createBalancedTree (elem size)
  (if (< size 3) (balancedStart elem size)
    (let ((nodesize (/ (1- size) 2))
          (sizemod2 (mod (1- size) 2)))
      (if (= 1 sizemod2) ; need alternately sized children
          (let ((child (create2 elem nodesize)))
            (list elem (car child) (cadr child))
          )
        ;; children have createBalancedTree sizes
        (let ((child (createBalancedTree elem nodesize)))
          (list elem child child)
        )
      )
    )
  )
)

(defun create2 (elem size)
  (list (createBalancedTree elem (1+ size)) (createBalancedTree elem size))
)

;; should be (1)
(complete 1 1)

;; should be (1 (1) (1))
(complete 1 2)

;; should be (1 (1 (1) (1)) (1 (1) (1)))
(complete 1 3)

;; should be (1)
(createBalancedTree 1 1)

;; should be (1 (1))
(createBalancedTree 1 2)

;; should be (1 (1 (1 (1)) (1)) (1 (1 (1)) (1)))
(createBalancedTree 1 9)
