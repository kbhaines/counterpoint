
(defparameter *consonant-intervals* '(0 4 7 12))

(defparameter *notes* '(c1 c1# d1 d1# e1 f1 f1# g1 g1# a1 a1# b1
                        c2 c2# d2 d2# e2 f2 f2# g2 g2# a2 a2# b2))

(defparameter *melody* '(0 2 1 3 4 5 4 1))


(defun notes->nums(notes)
  (mapcar (lambda (n) (position n *notes*)) notes))

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

(defun score-dissonance (melody notes)
  (loop for mel in melody
        for note in notes sum(if (member (- mel note) *consonant-intervals*) 0 -5)))

(defun apply-scores(melody p)
  (mapcar (lambda(f) (funcall f melody p)) '(score-dissonance)))

(defun score-pop(melody popu)
  (mapcar (lambda (p) (list (apply #'+ (apply-scores melody p)) p)) popu))

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

(defparameter e ())

(defun get-env()
  (setq e (environment *melody* *pop* 500)))

