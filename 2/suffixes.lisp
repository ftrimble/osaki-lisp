;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun suffixes (lst)
  (if (endp lst)
      '(())
    (cons lst (suffixes (cdr lst)))
    )
  )

(suffixes '(1 2 3 4))
(suffixes '())
