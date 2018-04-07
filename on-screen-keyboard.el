(defgroup on-screen-keyboard nil
  "Display a visual on-screen keyboard."
  :prefix "on-screen-keyboard-"
  :group 'convenience)

;; 0 is empty space
;; 1 is backspace
;; 2 is return
;; 3 is tab
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

(define-button-type 'on-screen-keyboard-key
  :supertype 'on-screen-keyboard-base
  'action (lambda (button) (message "key pressed")))

(define-button-type 'on-screen-keyboard-backspace
  :supertype 'on-screen-keyboard-base
  'action (lambda (button) (message "backspace pressed")))

(define-button-type 'on-screen-keyboard-return
  :supertype 'on-screen-keyboard-base
  'action (lambda (button) (message "return pressed")))

(define-button-type 'on-screen-keyboard-tab
  :supertype 'on-screen-keyboard-base
  'action (lambda (button) (message "tab pressed")))

(defun on-screen-keyboard-handle-key (key)
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
