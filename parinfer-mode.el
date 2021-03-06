(require 'parinferlib)

(defvar-local parinfer-mode--last-changes
  nil
  "Holds data on last changes made in buffer
If nil, there are no changes
Otherwise, it is a list whose car holds the cursor-dx value to pass to parinferlib")

(defvar-local parinfer-mode--tab-stops
  nil
  "Holds data on the tab stops on current line")

;; following variables are defined global for user experience. User
;; should not have to look up which mode of parinfer he is in when
;; switching buffers
(defvar parinfer-mode--current-mode
  :indent
  "Submode of parinfer; it is either :indent or :paren")

(defvar parinfer-mode--processor
  'parinferlib-indent-mode
  "Parinfer function to process current buffer")

(defconst parinfer-mode--col-interval
  2
  "Indent between levels of parenthesis")

(defun parinfer-mode--replace-line (index newline old-index)
  "Helper function
Replace line at index with newline, presuming currently we are on oldline"
  (forward-line (- index (symbol-value old-index)))
  (let ((beg (point))
        (end (progn (move-end-of-line 1) (point))))
    (insert newline)
    (delete-region beg end))
  (set old-index index))

(defun parinfer-mode--result-guard ()
  "Helper function
Returns t if cursor position allows to insert result"
  (not (and (= (or (char-before (point)) -1) 10)
            (= (or (char-after (point)) -1) 41))))

(defun parinfer-mode--insert-result (result)
  "Put results of running parinfer in current buffer
If cursor is right after a newline and next character is a closing curly brace, result is ignored
All lines that were changed are replaced, then cursor is set toa new position"
  (let ((inhibit-modification-hooks t)
        (tab-stops (plist-get result :tab-stops)))
    ;; first, old tab-stops are discarded
    (setq parinfer-mode--tab-stops nil)
    (if (plist-get result :success)
        (if (parinfer-mode--result-guard)
            (let* ((old-line (line-number-at-pos))
                   (new-lines (plist-get result :changed-lines))
                   (new-line-point (plist-get result :cursor-x))
                   (cur-line old-line))
              (setq parinfer-mode--tab-stops tab-stops)
              (mapc (lambda (elem)
                      (parinfer-mode--replace-line (1+ (plist-get elem :line-no))
                                                   (plist-get elem :line)
                                                   'cur-line))
                    new-lines)
              (forward-line (- old-line cur-line))
              (forward-char new-line-point)))
      (message "Parinfer did not succeed: %s" (plist-get result :error)))))

(defun parinfer-mode--compose-mode-line ()
  "Compose mode line for the current work mode"
  (format " Parinfer[%s]"
          (if (eq parinfer-mode--current-mode :indent)
              "Indent"
            "Paren")))

(defun parinfer-mode--toggle ()
  "Switch between parinfer submodes"
  (interactive)
  (if (eql parinfer-mode--current-mode :indent)
      (progn
        (setq parinfer-mode--current-mode :paren)
        (setq parinfer-mode--processor 'parinferlib-paren-mode))
     (setq parinfer-mode--current-mode :indent)
     (setq parinfer-mode--processor 'parinferlib-indent-mode))
  (force-mode-line-update))

(defun parinfer-mode--refresh ()
  "Process curret buffer"
  (interactive)
  (let ((options (list :cursor-x (current-column)
                       :cursor-line (1- (line-number-at-pos)))))
    (parinfer-mode--insert-result (funcall parinfer-mode--processor
                                           (buffer-string)
                                           options))))

(defun parinfer-mode--process-changes (beg end old)
  "Store cursor changes for later use"
  (unless undo-in-progress
    (let ((cursor-dx
           (if (= (line-number-at-pos beg)
                  (line-number-at-pos end))
               (or (and (>= (point) beg)
                        (> (- end beg) old)
                        (- end old beg))
                   (and (<= (point) end)
                        (> old (- end beg))
                        (- end old beg)))))
          (prev-dx (car parinfer-mode--last-changes)))
      (if parinfer-mode--last-changes
          (setcar parinfer-mode--last-changes (and cursor-dx
                                                   prev-dx
                                                   (+ cursor-dx prev-dx)))
        (setq parinfer-mode--last-changes (list cursor-dx))))))

(defun parinfer-mode--postprocess-changes ()
  "After command finishes executing, process all changes made there"
  (when parinfer-mode--last-changes
    (let ((options (list :cursor-x (current-column)
                         :cursor-line (1- (line-number-at-pos))
                         :cursor-dx (car parinfer-mode--last-changes))))
      (parinfer-mode--insert-result (funcall parinfer-mode--processor
                                             (buffer-string)
                                             options)))
    (setq parinfer-mode--last-changes nil)))

(defun parinfer-mode--cycle-indent ()
  "Indent the line if in indent mode"
  (interactive)
  (when (and parinfer-mode--tab-stops
             (eql parinfer-mode--current-mode :indent))
    (let* ((old-point (current-column))
           (old-indent (current-indentation))
           (stops parinfer-mode--tab-stops)
           (cur-stop (car stops))
           (target-indent
             (if (>= (plist-get cur-stop :x) old-indent)
                 (plist-get (car (last stops)) :x)
               (cl-dolist (next-stop stops (plist-get cur-stop :x))
                 (if (< (plist-get next-stop :x) old-indent)
                     (setq cur-stop next-stop)
                   (return (plist-get cur-stop :x)))))))
      (indent-line-to target-indent)
      (if (> old-point old-indent)
          (forward-char (- old-point old-indent))))))

(define-minor-mode parinfer-mode
  "Uses Parinfer to Format lispy code"
  :lighter (:eval (parinfer-mode--compose-mode-line))
  :keymap (let ((map (make-sparse-keymap)))
             (define-key map (kbd "C-c t") 'parinfer-mode--toggle)
             (define-key map (kbd "C-c r") 'parinfer-mode--refresh)
             (define-key map (kbd "<backtab>") 'parinfer-mode--cycle-indent)
             map)
  (if parinfer-mode
      ;; when bode is turned on
      (progn
        (parinfer-mode--refresh)
        (add-hook 'after-change-functions 'parinfer-mode--process-changes nil t)
        (add-hook 'post-command-hook 'parinfer-mode--postprocess-changes nil t))
   ;; when mode is being turned off
    (remove-hook 'after-change-functions 'parinfer-mode--process-changes t)
    (remove-hook 'post-command-hook 'parinfer-mode--postprocess-changes t)))

(defun parinfer-mode--debug-show-tabs ()
  (save-excursion
    (let* ((beg (progn (move-beginning-of-line 1)(point)))
           (end (progn (move-end-of-line 1) (point)))
           (line-length (- end beg)))
     (dolist (tab-el parinfer-mode--tab-stops)
       (put-text-property (+ beg (plist-get tab-el :x))
                          (+ beg (plist-get tab-el :x) 1)
                          'face '(:background "green"))))))

(provide 'parinfer-mode)
