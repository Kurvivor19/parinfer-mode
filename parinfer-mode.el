(require 'parinferlib)

(defvar parinfer-mode--last-changes
  nil
  "Holds data on last changes made in buffer
If nil, there are no changes
Otherwise, it is a list whose car holds the cursor-dx value to pass to parinferlib")

(defvar parinfer-mode--current-mode
  :indent
  "Submode of parinfer; it is either :indent or :paren")

(defvar parinfer-mode--processor
  'parinferlib-indent-mode
  "Parinfer function to process current buffer")

(defun parinfer-mode--replace-line (index newline old-index)
  "Helper function
Replace line at index with newline, presuming currently we are on oldline"
  (forward-line (- index (symbol-value old-index))
  (let ((beg (point))
        (end (progn (move-end-of-line 1) (point))))
    (insert newline)
    (delete-region beg end))
  (set old-index index)))

(defun parinfer-mode--result-guard ()
  "Helper function
Returns t if cursor position allows to insert result"
  (not (and (= (or (char-before (point)) -1) 10)
            (= (or (char-after (point)) -1) 41))))

(defun parinfer-mode--insert-result (result)
  "Put results of running parinfer in current buffer
If cursor is right after a newline and next character is a closing curly brace, result is ignored
All lines that were changed are replaced, then cursor is set toa new position"
  (let ((inhibit-modification-hooks t))
    (if (plist-get result :success)
        (if (parinfer-mode--result-guard)
            (let* ((old-line (line-number-at-pos))
                   (new-lines (plist-get result :changedLines))
                   (new-line-point (plist-get result :cursorX))
                   (cur-line old-line))
              (mapc (lambda (elem)
                      (parinfer-mode--replace-line (1+ (aref elem 0))
                                                   (aref elem 1)
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
  (parinfer-mode--insert-result (funcall parinfer-mode--processor (buffer-string)
                                         (current-column)
                                         (1- (line-number-at-pos))
                                         nil)))

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
    (parinfer-mode--insert-result (funcall parinfer-mode--processor (buffer-string)
                                           (current-column)
                                           (1- (line-number-at-pos))
                                           (car parinfer-mode--last-changes)))
    (setq parinfer-mode--last-changes nil)))

(define-minor-mode parinfer-mode
  "Uses Parinfer to Format lispy code"
  :lighter (:eval (parinfer-mode--compose-mode-line))
  :keymap (let ((map (make-sparse-keymap)))
             (define-key map (kbd "C-c t") 'parinfer-mode--toggle)
             (define-key map (kbd "C-c r") 'parinfer-mode--refresh)
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

(provide 'parinfer-mode)
