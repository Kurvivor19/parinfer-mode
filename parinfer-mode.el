(load-file "./parinferlib.el")

(defun parinfer-mode--insert-result (result)
  (let ((inhibit-modification-hooks t))
    (cl-macrolet ((-char-before (point) `(or (char-before ,point) -1))
                  (-char-after (point) `(or (char-after ,point) -1)))
      (if (and (plist-get result :success)
               (not (and (= (-char-before (point)) 10)
                         (= (-char-after (point)) 41))))
          (let ((old-buffer (current-buffer))
                (old-point (point)))
            (with-temp-buffer
              (insert (plist-get result :text))
              (copy-to-buffer old-buffer (point-min) (point-max)))
            (goto-char old-point))))))

(defun parinfer-mode-indent-mode ()
  (parinfer-mode--insert-result (parinferlib-indent-mode (buffer-string)
                                                        (current-column)
                                                        (- (line-number-at-pos) 1)
                                                        nil)))

(defun parinfer-mode-paren-mode ()
  (parinfer-mode--insert-result (parinferlib-paren-mode (buffer-string)
                                                       (current-column)
                                                       (- (line-number-at-pos) 1)
                                                       nil)))

(defvar parinfer-mode--current-mode
  :indent
  "Submode of parinfer; it is either :indent or :paren")

(defvar parinfer-mode--processor
  'parinferlib--indent-mode
  "Parinfer function to process current buffer")

(defun parinfer-mode--compose-mode-line ()
  "Compose mode line for the current work mode"
  (format " Parinfer[%s]"
          (if (eq parinfer-mode--current-mode :indent)
              "Indent"
            "Paren")))

(defun parinfer-mode--toggle ()
  "Switch between parinfer submodes"
  (interactive)
  (case parinfer-mode--current-mode
    (:indent
     (setq parinfer-mode--current-mode :paren)
     (setq parinfer-mode--processor 'parinferlib-paren-mode))
    (t
     (setq parinfer-mode--current-mode :indent)
     (setq parinfer-mode--processor 'parinferlib-indent-mode)))
  (force-mode-line-update))

(defun parinfer-mode--refresh ()
  "Process curret buffer"
  (interactive)
  (parinfer-mode--insert-result (funcall parinfer-mode--processor (buffer-string)
                                         (current-column)
                                         (1- (line-number-at-pos))
                                         nil)))

(defun parinfer-mode--process-changes (BEG END OLD)
  (let ((cursor-dx
         (if (= (line-number-at-pos BEG)
                (line-number-at-pos END))
             (or (and (>= (point) BEG)
                      (> (- END BEG) OLD)
                      (- END OLD BEG))
                 (and (<= (point) END)
                      (> OLD (- END BEG))
                      (- END OLD BEG))))))
    (parinfer-mode--insert-result (funcall parinfer-mode--processor (buffer-string)
                                           (current-column)
                                           (1- (line-number-at-pos))
                                           cursor-dx))))

(define-minor-mode parinfer-mode
  "Uses Parinfer to Format lispy code"
  :lighter (:eval (parinfer-mode--compose-mode-line))
  :keymap (let ((map (make-sparse-keymap)))
             (define-key map (kbd "C-c t") 'parinfer-mode--toggle)
             (define-key map (kbd "C-c r") 'parinfer-mode--refresh)
             map)
  (cond
   ;; when mode is being turned on
   (parinfer-mode
    (parinfer-mode--refresh)
    (add-hook 'after-change-functions 'parinfer-mode--process-changes nil t))
   ;; when mode is being turned off
   (t
    (remove-hook 'after-change-functions 'parinfer-mode--process-changes t))))

(provide 'parinfer-mode)
