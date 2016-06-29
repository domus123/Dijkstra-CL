;;This code was writed by Lucas Guerra
;;CANNOT be charged
;;Contact lu.guerra7508@gmail.com
;;Solve ANY graph in the same model of *nodes*
;;Cheers lispers
;;Backup


(defparameter *nodes* '((A (A B . 85) (A E . 173) (A C . 217))
			(B (B F . 80))
			(C (C A . 217) (C G . 186) (C H . 103))
			(D (D H . 183))
			(E (E A . 173) (E J . 502))
			(F (F B . 80) (F I . 250))
			(G (G C . 186))
			(H (H J . 167) (H D . 183) (H C . 103))
			(I (I F . 250) (I J . 84))
			(J (J I . 84) (J H . 167) (J E . 502))))
(defparameter result nil)
(defun reset-res()
  (setq result nil))

;;Return all paths that cur can go
(defun get-nodes (cur)
  (let* ( ( lst (cdr (assoc cur *nodes*)))
	 (s-lst (sort lst '< :key #'cddr)))
    s-lst))
;;Main function
;;search ALL possible combinations and save those who have founded the goal

(defun cp (cur  goal  &optional (val 0)  (lst nil))
  (if (equal cur goal) (progn  (push (list (reverse lst) val) result))
      (loop for items in (get-nodes cur)
	 do (cond ( (member (cadr items) lst) '())
		  ( t  (cp (cadr items) goal (+ val (cddr items)) (append (list (cadr items)) lst))) ))) )
		 

;;Start funtion
(defun sp (init goal )
  (reset-res)
  (cp init goal 0 (list init))
  (let ( ( better (sort result '< :key #'cadr)))
    (format t "Todos os caminhos ~A~%" better)
    (format t "Sequencia ~A Peso ~A ~%" (caar better) (cdar better)) ))


(defun test ()
  (loop for x in '(A B C D E F G H I)
     do(progn (format t "~&-----------------------------~A-----------------------------------------~&" x)
	      (loop for y in '(A B C D E F C H I)
		 do (sp x y))) ))
