
(defparameter *consonant-intervals* '(0 4 7 12))

(defparameter *notes* '(c1 c#1 d1 d#1 e1 f1 f#1 g1 g#1 a1 a#1 b1
                        c2 c#2 d2 d#2 e2 f2 f#2 g2 g#2 a2 a#2 b2))

(defparameter *melody* '(d2 f#2 e2 d2 a2 d2 f#2 e2 d2 g2))
;(defparameter *melody* '(e2 e2 d2 d2 d2 e2 d2 g2))


(defun notes->nums(notes)
  (mapcar (lambda (n) (position n *notes*)) notes))

(defun nums->notes(nums)
  (mapcar (lambda (n) (nth n *notes*)) nums))

(defun contra-rnd () (loop for i below (length *melody*) collect (random (length *notes*))))

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

(defun calc-movement(notes)
  (loop for (n1 n2) on notes when n2 collect (- n2 n1)))

(defun score-contrary(melody counterpoint)
  (let* ((melody-movement (calc-movement melody))
         (cp-movement (calc-movement counterpoint)))
    (apply #'+ (mapcar #'- melody-movement cp-movement))))

(defun score-movement(melody notes)
  (if (every (lambda(x) (< x 4)) (mapcar #'abs (calc-movement notes)))
    0 -10))

(defun apply-scores(melody p)
  (mapcar (lambda(f) (funcall f melody p)) '(score-contrary score-dissonance score-movement)))

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
  (setq e (environment (notes->nums *melody*) *pop* 500)))

(defun run-it(&optional &key init)
  (if init (get-env))
  (loop repeat 500 do (funcall e))
  (print *melody*)
  (print (nums->notes(car (funcall e)))))
