(defgroup viskbd nil
  "Display a visual on-screen keyboard."
  :prefix "viskbd-"
  :group 'convenience)

;; 0 is empty space
;; 1 is backspace
;; 2 is return
;; 3 is tab
(setq viskbd-keymap [[?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P 0 1]
		     [?A ?S ?D ?F ?G ?H ?J ?K ?L 0 0 2]
		     [?Z ?X ?C ?V ?B ?N ?M 0 0 0 0 3]])

(defvar viskbd-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'push-button)
    map)
  "The button map of the visual keyboard keys.")

(define-button-type 'viskbd-base
  'help-echo (purecopy "mouse-1, RET: Push this button")
  'keymap viskbd-button-map
  'face 'tool-bar
  'mouse-face 'tool-bar)

(define-button-type 'viskbd-key
  :supertype 'viskbd-base
  'action (lambda (button) (message "key pressed")))

(define-button-type 'viskbd-backspace
  :supertype 'viskbd-base
  'action (lambda (button) (message "backspace pressed")))

(define-button-type 'viskbd-return
  :supertype 'viskbd-base
  'action (lambda (button) (message "return pressed")))

(define-button-type 'viskbd-tab
  :supertype 'viskbd-base
  'action (lambda (button) (message "tab pressed")))

(defun viskbd-handle-key (key)
  (cond ((= key 0) (insert " "))
	((= key 1) (insert-button "BSP" :type 'viskbd-backspace))
	((= key 2) (insert-button "RET" :type 'viskbd-return))
	((= key 3) (insert-button "TAB" :type 'viskbd-tab))
	(t (insert-button key :type 'viskbd-key))))

(defun viskbd-display (keymap)
  "Display the keymap."
  (mapcar (lambda (row) (mapcar 'viskbd-handle-key row) (insert "\n")) keymap))

(defun viskbd ()
  "Display a visual on-screen keyboard."
  (interactive)
  (let ((buffer (get-buffer-create "*viskbd*")))
    (pop-to-buffer buffer)
    (viskbd-display viskbd-keymap)
    (setq buffer-read-only t)))
