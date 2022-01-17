;; me(my evil) mode
;; simplified vim command

(defun me-dummy-bind ()
	(interactive)
	(message "dummy key"))

(defvar me-mode-hook nil)

(defvar me-local-mode-map
	(make-keymap))

(define-minor-mode me-local-mode
	"simplified vim minor mode"
	:init-value nil
	:global 1
	:lighter " me"
	:keymap me-local-mode-map)

(defvar-local me-pre-keystrokes '()
	"record pre keystrokes to perform next operation")

(defvar-local me-line-selection-overlay nil
	"the overlay of just line selection")

(defvar-local me-visual-begin-pos nil
	"begin pos of the visual line")

(defvar-local me-visual-end-pos nil
	"end pos of the visual line")

(defvar-local me-paste-new-line nil
	"open new line and paste")

(defun me-test-keys ()
	(message "keys %s" me-pre-keystrokes))

(defun me-clear-keystrokes ()
	"clear saved keystrokes"
	(setq me-pre-keystrokes '()))

(defun me-advice-clear-everything ()
	(me-clear-keystrokes)
	;;(message "clear keystrokes")
	(if me-line-selection-overlay
			(delete-overlay me-line-selection-overlay))
	(setq me-line-selection-overlay nil)
	(setq me-visual-begin-pos nil)
	(setq me-visual-end-pos nil)
	(setq mark-active nil)
	(setq me-paste-new-line nil))



(defun me-recalc-visual-pos-next-line ()
	"only under visual line selection"
	;;(message "%s %s %s" (line-beginning-position) me-visual-begin-pos me-visual-end-pos)
	(if (and (> (line-beginning-position) me-visual-begin-pos)
					 (< (line-beginning-position) me-visual-end-pos))
			(setq me-visual-begin-pos (line-beginning-position))
		(setq me-visual-end-pos (line-end-position))))

(defun me-recalc-visual-pos-previous-line ()
	"only under visual line selection"
	(if (and (> (line-end-position) me-visual-begin-pos)
					 (< (line-end-position) me-visual-end-pos))
			(setq me-visual-end-pos (line-end-position))
		(setq me-visual-begin-pos (line-beginning-position))))

(defun me-advice-next-line (&rest r)
	"move next line check visual selection"
	(if me-line-selection-overlay
			(progn
				(me-recalc-visual-pos-next-line)
				(move-overlay me-line-selection-overlay me-visual-begin-pos me-visual-end-pos)
				(overlay-put me-line-selection-overlay 'face 'region))))

(defun me-advice-previous-line (&rest r)
	"move previous line check visual selection"
	(if me-line-selection-overlay
			(progn
				(me-recalc-visual-pos-previous-line)
				(move-overlay me-line-selection-overlay me-visual-begin-pos me-visual-end-pos)
				(overlay-put me-line-selection-overlay 'face 'region))))

(defun me-advice-move-beginning (&rest r)
	(indent-for-tab-command))

(defun me-advice-tab (&rest r)
	(if me-line-selection-overlay
			(indent-region me-visual-begin-pos me-visual-end-pos)))

(defun me-add-all-advice ()
	(advice-add #'keyboard-quit :before #'me-advice-clear-everything)
	(advice-add #'next-line :after #'me-advice-next-line)
	(advice-add #'previous-line :after #'me-advice-previous-line)
	(advice-add #'indent-for-tab-command :after #'me-advice-tab))

(defun me-remove-all-advice ()
	(advice-remove #'keyboard-quit  #'me-advice-clear-everything)
	(advice-remove #'next-line  #'me-advice-next-line)
	(advice-remove #'previous-line  #'me-advice-previous-line)
	(advice-remove #'indent-for-tab-command  #'me-advice-tab))

(defmacro me-push-keystrokes (k)
	"the macro save keystrokes to later action"
	`(lambda ()
		 (interactive)
		 (push ,k me-pre-keystrokes)
		 (message "saved keystrokes %s" me-pre-keystrokes)
		 (if (me-trigger-operation ,k)
				 (me-clear-keystrokes))))

(defun me-refresh-visual-pos ()
	"update the var's value"
	(if me-visual-begin-pos
			(let ((line-begin-pos-now (line-beginning-position)))
				(setq me-visual-begin-pos (if (< line-begin-pos-now me-visual-begin-pos)
																	 line-begin-pos-now
																 me-visual-begin-pos)))
		(setq me-visual-begin-pos (line-beginning-position)))
	(if me-visual-end-pos
			(let ((line-end-pos-now (line-end-position)))
				(setq me-visual-end-pos (if (> line-end-pos-now me-visual-end-pos)
																	 line-end-pos-now
																 me-visual-end-pos)))
		(setq me-visual-end-pos (line-end-position))))

(defun me-make-line-visual-selection ()
	"start line visual selection"
	(interactive)
	(me-advice-clear-everything)
	(me-refresh-visual-pos)
	(if me-line-selection-overlay
			(move-overlay me-line-selection-overlay me-visual-begin-pos me-visual-end-pos)
		(setq me-line-selection-overlay
					(make-overlay me-visual-begin-pos me-visual-end-pos)))
	(overlay-put me-line-selection-overlay 'face 'region)
	(me-setmark-line-visual-selection))

(defun me-trigger-operation (arg)
	"trigger multi key operation"
	(cond ((eq 'd arg) (or (me-trigger-dd)
												 (me-trigger-d-region-kill)))
				((eq 'w arg) (me-trigger-dw))
				((eq 'y arg) (or (me-trigger-yy)
												 (me-trigger-y-region-copy)))))

(defun me-trigger-dd ()
	"check if can trigger dd operation"
	(let ((empty-line? (eq (line-beginning-position)
												 (line-end-position))))
		(ignore-errors
			(move-beginning-of-line nil)
			(kill-line))
		(if (not empty-line?)
				(delete-char 1))))

(defun me-trigger-dw ()
	"kill one word"
	(ignore-errors
		(kill-region (point)
								 (progn
									 (forward-word)
									 (point)))))

(defun me-trigger-y-region-copy ()
	"if mark-active copy"
	(if mark-active
			(progn
				(kill-ring-save (region-beginning) (region-end))
				t)))

(defun me-p-operation ()
	"do p operation = yank"
	(interactive)
	(move-end-of-line nil)
	(newline)
	(yank))

(defun me-upper-p-operation ()
	"do P operation = yank"
	(interactive)
	(move-previous-line-new-line)
	(yank))

(defun me-move-beginning ()
	"move beginning and tab"
	(interactive)
	(move-beginning-of-line nil)
	(indent-for-tab-command))

(defun me-1-bind ()
	"bind 1 to c-x 1"
	(interactive)
	(delete-other-windows))

(defun me-2-bind ()
	"bind 2 to c-x 2"
	(interactive)
	(split-window-below)
	(other-window 1))

(defun me-3-bind ()
	"bind 3 to c-x 3"
	(interactive)
	(split-window-right)
	(other-window 1))

(defun me-line-end-insert ()
	(interactive)
	(move-end-of-line nil)
	(me-mode-disable))

(defun me-o-bind ()
	(interactive)
	(move-end-of-line nil)
	(newline)
	(indent-for-tab-command)
	(me-mode-disable))

(defun me-forward-char-insert ()
	(interactive)
	(forward-char)
	(me-mode-disable))

(defun me-move-previous-line-new-line ()
	"move to previous line check first line"
	(if (= 1 (line-number-at-pos))
			(progn
				(move-beginning-of-line nil)
				(newline)
				(previous-line nil))
		(progn
			(previous-line nil)
			(move-end-of-line nil)
			(newline))))

(defun me-O-bind ()
	(interactive)
	(me-move-previous-line-new-line)
	(indent-for-tab-command)
	(me-mode-disable))

(defun me-kill-whole-line (&optional arg)
	(interactive)
	(move-beginning-of-line arg)
	(kill-line arg)
	(kill-line arg))

(defun me-initialize ()
	(unless (or (minibufferp)
							(not (equal (key-binding "k") 'self-insert-command)))
		(me-local-mode 1)
		(me-clear-keystrokes)))

(define-globalized-minor-mode	me-mode me-local-mode	me-initialize)

(defun me-mode-disable ()
	"Disable my evil mode on special occation"
	(interactive)
	(me-local-mode -1)
	(me-advice-clear-everything)
	(me-remove-all-advice))

(defun me-mode-enable ()
	"auto enable my evil mode when evil mode"
	(interactive)	
	(if (and (not (bound-and-true-p me-local-mode))
					 (/= (point) (line-beginning-position)))
			(backward-char))
	(me-local-mode 1)
	(me-add-all-advice))

(defun define-me-key-push (kbd-k k)
	(define-key me-local-mode-map (kbd kbd-k) (me-push-keystrokes k)))

(defun me-w-bind ()
	"when press w"
	(interactive)
	(cond ((null (car me-pre-keystrokes))
				 (forward-word))
				((eq 'd (car me-pre-keystrokes))
				 (me-trigger-dw)
				 (me-advice-clear-everything))
				((eq 'c (car me-pre-keystrokes))
				 (me-trigger-dw)
				 (me-advice-clear-everything)
				 (me-mode-disable))))

(defun me-y-bind ()
	"when press y"
	(interactive)
	(cond	(mark-active ;; by set-mark-command
				 (progn
					 (kill-ring-save (region-beginning) (region-end))
					 (me-advice-clear-everything)))
				(me-line-selection-overlay ;; by big V
				 (progn
					 (kill-ring-save me-visual-begin-pos me-visual-end-pos)
					 (me-advice-clear-everything)))
				((eq 'y (car me-pre-keystrokes)) ;; yy action dont clear keystrokes
				 (push 'y me-pre-keystrokes)
				 (kill-ring-save (line-beginning-position) (line-end-position))
				 (setq me-paste-new-line t))
				(t ;; for yy
 				 (push 'y me-pre-keystrokes))))

(defun me-p-bind ()
	"when press p"
	(interactive)
	(message "new paste line %s" me-paste-new-line)
	(if me-paste-new-line
			(progn
				(move-end-of-line nil)
				(newline)
				(move-beginning-of-line nil)))
	(yank))

(defun me-esc-bind ()
	"when press esc"
	(interactive)
	(me-advice-clear-everything))

(defun me-x-bind ()
	"when press x"
	(interactive)
	(cond	(mark-active ;; by set-mark-command
				 (kill-region (region-beginning) (region-end)))
				(me-line-selection-overlay ;; by big V
				 (progn
					 (kill-region me-visual-begin-pos me-visual-end-pos)
					 (me-advice-clear-everything)))
				(t
				 (delete-char 1))))

(defun me-c-bind ()
	"push c into keystrokes"
	(interactive)
	(push 'c me-pre-keystrokes))

(defun me-d-bind ()
	"when press d"
	(interactive)
	(if (null (car me-pre-keystrokes)) ;; no pre keys
			(cond	(mark-active ;; by set-mark-command
						 (kill-region (region-beginning) (region-end))
						 (setq me-paste-new-line nil))
						(me-line-selection-overlay ;; by big V
						 (kill-region me-visual-begin-pos me-visual-end-pos)
						 (me-advice-clear-everything))
						(t
						 (push 'd me-pre-keystrokes)))
		(if (eq 'd (car me-pre-keystrokes)) ;; double d
				(progn
					(me-trigger-dd)
					(message "new paste line set t")
					(setq me-pre-keystrokes '())	
					(setq me-paste-new-line t)))))

(defun me-v-bind ()
	"when press v"
	(interactive)
	(me-advice-clear-everything)
	(set-mark-command nil))

(defun me-left-bracket-bind ()
	"when press ("
	(interactive)
	(if (equal ?\) (char-before (point)))
			(backward-sexp)))

(defun me-right-bracket-bind ()
	"when press )"
	(interactive)
	(if (equal ?\( (char-after (point)))
			(forward-sexp)))

(defun me-return-bind ()
	"when press return"
	(interactive)
	(if (and (equal ?\} (char-after (point)))
					 (equal ?\{ (char-before (point))))
			(progn
				(newline)
				(indent-for-tab-command)
				(me-O-bind))))

(defun me---bind ()
	"when press -"
	(interactive)
	(other-window 1))

(defun me-r-bind ()
	"when press r"
	(interactive)
	(me-x-bind)
	(me-mode-disable))

(defun me-backspace-bind ()
	"when press backspace"
	(interactive)
	(while (or (eq ?  (char-before (point)))
						 (eq ?\t (char-before (point)))
						 (eq ?\r (char-before (point)))
						 (eq ?\n (char-before (point))))
		(backward-delete-char 1 nil)))

;; implemented command
(define-key me-local-mode-map [escape] 'me-esc-bind)
(define-key me-local-mode-map (kbd "0") 'me-move-beginning)
(define-key me-local-mode-map (kbd "1") 'me-1-bind)
(define-key me-local-mode-map (kbd "2") 'me-2-bind)
(define-key me-local-mode-map (kbd "3") 'me-3-bind)
(define-key me-local-mode-map (kbd "a") 'me-forward-char-insert)
(define-key me-local-mode-map (kbd "A") 'me-line-end-insert)
(define-key me-local-mode-map (kbd "b") 'backward-word)
(define-key me-local-mode-map (kbd "c") 'me-c-bind)
(define-key me-local-mode-map (kbd "d") 'me-d-bind)
(define-key me-local-mode-map (kbd "e") 'move-end-of-line)
(define-key me-local-mode-map (kbd "h") 'backward-char)
(define-key me-local-mode-map (kbd "i") 'me-mode-disable)
(define-key me-local-mode-map (kbd "j") 'next-line)
(define-key me-local-mode-map (kbd "k") 'previous-line)
(define-key me-local-mode-map (kbd "l") 'forward-char)
(define-key me-local-mode-map (kbd "o") 'me-o-bind)
(define-key me-local-mode-map (kbd "O") 'me-O-bind)
(define-key me-local-mode-map (kbd "p") 'me-p-bind)
(define-key me-local-mode-map (kbd "P") 'me-upper-p-operation)
(define-key me-local-mode-map (kbd "r") 'me-r-bind)
(define-key me-local-mode-map (kbd "v") 'me-v-bind)
(define-key me-local-mode-map (kbd "V") 'me-make-line-visual-selection)
(define-key me-local-mode-map (kbd "w") 'me-w-bind)
(define-key me-local-mode-map (kbd "W") 'forward-word)
(define-key me-local-mode-map (kbd "x") 'me-x-bind)
(define-key me-local-mode-map (kbd "u") 'undo)
(define-key me-local-mode-map (kbd "y") 'me-y-bind)
(define-key me-local-mode-map (kbd "<") 'beginning-of-buffer)
(define-key me-local-mode-map (kbd ">") 'end-of-buffer)
(define-key me-local-mode-map (kbd "$") 'move-end-of-line)
(define-key me-local-mode-map (kbd "(") 'me-left-bracket-bind)
(define-key me-local-mode-map (kbd ")") 'me-right-bracket-bind)
(define-key me-local-mode-map (kbd "RET") 'me-return-bind)
(define-key me-local-mode-map (kbd "-") 'me---bind)
(define-key me-local-mode-map (kbd "DEL") 'me-backspace-bind)

;; ignore command
(define-key me-local-mode-map (kbd "4") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "5") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "6") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "7") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "8") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "9") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "B") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "C") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "D") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "E") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "f") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "F") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "g") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "G") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "H") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "I") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "J") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "K") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "L") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "m") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "M") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "n") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "N") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "q") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "Q") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "R") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "s") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "S") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "t") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "T") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "U") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "X") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "Y") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "z") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "Z") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "SPC") 'me-dummy-bind)
(define-key me-local-mode-map (kbd ";") 'me-dummy-bind)
(define-key me-local-mode-map (kbd ":") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "/") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "?") 'me-dummy-bind)
(define-key me-local-mode-map (kbd ",") 'me-dummy-bind)
(define-key me-local-mode-map (kbd ".") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "'") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "\"") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "[") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "{") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "]") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "}") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "_") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "+") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "=") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "\\") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "|") 'me-dummy-bind)
(define-key me-local-mode-map (kbd "#") 'me-dummy-bind)

;;(global-set-key (kbd "M-i") 'me-mode)

(add-hook 'minibuffer-setup-hook 'me-mode-disable)
(add-hook 'minibuffer-exit-hook 'me-mode-enable)

(provide 'me-mode)
