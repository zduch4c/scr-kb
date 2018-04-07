;; -*- lexical-binding: t -*-

(defgroup viskbd nil
  "Display a visual on-screen keyboard."
  :prefix "viskbd-"
  :group 'convenience)

(defcustom viskbd-layout-regular '((?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?- ?= 0 ?\b)
				   (?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?[ ?] 0 0 ?\r)
				   (?a ?s ?d ?f ?g ?h ?j ?k ?l ?\; ?' ?\\ 0 0 ?\t)
				   (?z ?x ?c ?v ?b ?n ?m ?, ?. ?/ 1 0)
				   (?\s))
  "The regular layout."
  :group 'viskbd)

(defcustom viskbd-layout-shifted '((?~ ?! ?@ ?# ?$ ?% ?^ ?& ?* ?( ?) ?_ ?+ 0 ?\b)
				   (?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P ?{ ?} 0 0 ?\r)
				   (?A ?S ?D ?F ?G ?H ?J ?K ?L ?: ?\" ?| 0 0 ?\t)
				   (?Z ?X ?C ?V ?B ?N ?M ?< ?> ?? 1 0)
				   (?\s))
  "The layout when shift is pressed."
  :group 'viskbd)

(defcustom viskbd-keyboard-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] 'push-button)
    (define-key map [mouse-1] 'push-button)
    map)
  "Keymap used by buttons."
  :group 'viskbd)

(defvar shifted-flag nil
  "Is the shift key pressed?")

(define-button-type 'viskbd-shift
  'keymap viskbd-keyboard-map
  'face 'custom-button-mouse
  'action (lambda (button)
	    (if (not shifted-flag) (progn (setf shifted-flag t) (viskbd-display-kbd layout-shifted))
	      (progn (setf shifted-flag nil) (viskbd-display-kbd layout-regular)))))
(define-button-type 'viskbd-return
  'keymap viskbd-keyboard-map
  'face 'custom-button-mouse
  'action (lambda (button) (other-window 1) (newline) (other-window -1)))
(define-button-type 'viskbd-backspace
  'keymap viskbd-keyboard-map
  'face 'custom-button-mouse
  'action (lambda (button) (other-window 1) (backward-delete-char-untabify 1) (other-window -1)))
(define-button-type 'viskbd-tab
  'keymap viskbd-keyboard-map
  'face 'custom-button-mouse
  'action (lambda (button) (other-window 1) (indent-for-tab-command) (other-window -1)))

(defun viskbd-display-kbd (layout)
  (when buffer-read-only (setf buffer-read-only nil))
  (erase-buffer)
  (dolist (row layout)
    (dolist (key row)
      (define-button-type 'temporary-normal-key
	'keymap viskbd-keyboard-map
	'face 'custom-button-mouse
      	'action (lambda (button) (other-window 1) (insert key) (other-window -1)))
      (cond ((= key 0) (insert "   "))
	    ((= key 1) (insert-button "  SHIFT  " :type 'viskbd-shift))
	    ((= key ?\r) (insert-button " RET " :type 'viskbd-return))
	    ((= key ?\s) (insert-button "                   SPC                 " :type 'temporary-normal-key))
	    ((= key ?\b) (insert-button " BSP " :type 'viskbd-backspace))
	    ((= key ?\t) (insert-button " TAB " :type 'viskbd-tab))
	    (t (insert-button (format " %c " key) :type 'temporary-normal-key :face 'custom-button))))
    (insert "\n"))
  (when (not buffer-read-only) (setf buffer-read-only t)))

(defun viskbd ()
  (interactive)
  (let ((buffer (get-buffer-create "*viskbd*")))
    (pop-to-buffer buffer)
    (viskbd-display-kbd layout-regular)))
