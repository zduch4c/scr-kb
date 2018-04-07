;; -*- lexical-binding: t -*-

(defgroup on-screen-keyboard nil
  "Display a visual on-screen keyboard."
  :prefix "on-screen-keyboard-"
  :group 'convenience)

(setq on-screen-keyboard-keymap [[?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P 0 1]
				 [?A ?S ?D ?F ?G ?H ?J ?K ?L 0 0 2]
				 [?Z ?X ?C ?V ?B ?N ?M 0 0 0 0 3]])

(defvar on-screen-keyboard-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'push-button)
    map)
  "The button map of the on screen keyboard keys.")

(define-button-type 'on-screen-keyboard-base
  'help-echo (purecopy "mouse-1, RET: Push this button")
  'keymap on-screen-keyboard-button-map
  'face 'tool-bar
  'mouse-face 'tool-bar)

(define-button-type 'on-screen-keyboard-backspace
  :supertype 'on-screen-keyboard-base
  'action (lambda (button) (on-screen-keyboard-switch-do (lambda () (backward-delete-char-untabify 1)))))

(define-button-type 'on-screen-keyboard-return
  :supertype 'on-screen-keyboard-base
  'action (lambda (button) (on-screen-keyboard-switch-do (lambda () (newline)))))

(define-button-type 'on-screen-keyboard-tab
  :supertype 'on-screen-keyboard-base
  'action (lambda (button) (on-screen-keyboard-switch-do (lambda () (indent-for-tab-command)))))

(defun on-screen-keyboard-switch-do (func)
  "Switch to the last window, call the function, go back."
  (other-window 1)
  (funcall func)
  (other-window -1))

(defun on-screen-keyboard-handle-key (key)
  "Do the action of the given key."
  (define-button-type 'on-screen-keyboard-key
    :supertype 'on-screen-keyboard-base
    'action (lambda (button) (on-screen-keyboard-switch-do (lambda () (insert key)))))
  (cond ((= key 0) (insert " "))
	((= key 1) (insert-button "BSP" :type 'on-screen-keyboard-backspace))
	((= key 2) (insert-button "RET" :type 'on-screen-keyboard-return))
	((= key 3) (insert-button "TAB" :type 'on-screen-keyboard-tab))
	(t (insert-button key :type 'on-screen-keyboard-key))))

(defun on-screen-keyboard-display (keymap)
  "Display the keymap."
  (mapcar (lambda (row) (mapcar 'on-screen-keyboard-handle-key row) (insert "\n")) keymap))

(defun on-screen-keyboard ()
  "Display a visual on-screen keyboard."
  (interactive)
  (let ((buffer (get-buffer-create "*on-screen-keyboard*")))
    (pop-to-buffer buffer)
    (on-screen-keyboard-display on-screen-keyboard-keymap)
    (setq buffer-read-only t)))
