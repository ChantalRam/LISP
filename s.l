(defun surround-recursive(x y z)
  (cond  ;;if we're at the last element or something else is wrong with y
    ((or(null y) (not(listp y))) nil)
    (t ;;else recurse
      (cons (list x (car y) z)
        (surround-recursive x (cdr y) z)))))


(defun surround-iterative(x y z)
  (cond  ;;if we're at the last element or something else is wrong with y
    ((or(null y) (not(listp y))) nil)
    (t (do ((n (length y) (- n 1))
            (cur y (cdr cur))
            (a () (append a (list (list x (car cur) z)))))
           ((= 1 n) (append a (list (list x (car cur) z)))))))) ;(prin1 (list x (car cur) z)) ) ))) ;;end condition

(defun surround-mapcar(x y z)
  (cond
    ((or(null y) (not(listp y))) nil)
    (t (mapcar #'(lambda (i) (list x i z)) y))))


(defun max-min(l)
  (cond
    ((not(listp l)) nil)
    (t (list (maxlist l))))) ;(minlist l)))))

(defun maxlist (y)
   (cond ((null y) 9999)
   (t (max (cond
               ((integerp y) y)
               ((listp y) (maxlist(car y)))
;               (t (maxlist(car y)))
               (t nil)
            )
            (maxlist (cdr y))))))

;(defun minlist (y)
;   (cond ((null y) -9999)
;   (t (min (cond
;               ((integerp y) y)
;               ((and (not(atom y)) (integerp (car y))) (car y))
;               (t (minlist(car y))
;            )
;            (minlist (cdr q)))))))
