;; -*- lexical-binding: t -*-

(setf layout '((?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?- ?=)
	       (?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?[ ?])
	       (?a ?s ?d ?f ?g ?h ?j ?k ?l ?\; ?' ?\\)
	       (?z ?x ?c ?v ?b ?n ?m ?, ?. ?/)))

(defun viskbd-display-kbd ()
  (dolist (row layout)
    (dolist (key row)
      (define-button-type 'temporary
	'action (lambda (button)
		  (other-window 1)
		  (insert key)
		  (other-window -1)))
      (insert-button (format " %c " key) :type 'temporary))
    (insert "\n")))

(defun viskbd ()
  (interactive)
  (let ((buffer (get-buffer-create "*viskbd*")))
    (pop-to-buffer buffer)
    (viskbd-display-kbd)
    (read-only-mode)))
