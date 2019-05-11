
(defparameter *melody* '(0 2 1 3 4 5 4 1))

(defun contra-rnd () (loop for i below 8 collect (random 12)))

(defparameter *pop* (loop repeat 6 collect (contra-rnd)))

(defun pairs-from(l)
    (if l (cons (list (car l) (cadr l)) (pairs-from (cddr l)))
        ()))

(defun ranked-pop(melody popu)
  (apply #'append (mapcar #'cdr (sort (score-pop melody popu) (lambda (x y) (> (car x) (car y)))))))

(defun score-distance (melody line)
  (loop for mel in melody
        for lin in line
        sum (- (abs (- mel lin)))))

(defun score-notes(melody cp-line)
  (cond ((some (lambda (c) (or (< c 0) (> c 12))) cp-line) -100)
        (t 0)))


(defun score-pop(melody popu)
  (mapcar (lambda (p) (list (+ (score-distance melody p) (score-notes melody p)) p)) popu))

(defun clamp (val low high)
  (cond ((< val low) low)
        ((> val high) high)
        (t val)))

(defun mutate-one(seq)
  (let ((gene (random (length seq)))
        (mutation (clamp (- 50 (random 100)) -2 2))
        (mutant (copy-list seq)))
    (setf (nth gene mutant) (clamp (+ mutation (nth gene mutant)) 0 12))
    mutant))

(defun next-generation(popu max-germ-bags)
  (loop for (p1 p2) on popu
        for limit from 0 below max-germ-bags
        when p2 collect (offspring p1 p2)))

(defun offspring(par1 par2)
  (let ((child (append (mutate-one(subseq par1 0 (/ (length par1) 2)))
          (mutate-one(subseq par2 (/ (length par2) 2) (length par2))))))
    child))

(defun time-step(melody popu max-pop)
  (let*((popu (ranked-pop melody popu))
       (germ-bags (next-generation popu (/ (length popu) 1)))
       (new-pop (ranked-pop melody (append germ-bags popu))))
    (subseq new-pop 0 (min max-pop (length new-pop)))))

(defun environment(melody popu max-pop)
  (let((env-pop (copy-list popu)))
    (lambda ()
      (setf env-pop (time-step melody env-pop max-pop))
      env-pop)))
