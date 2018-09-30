;;; rasid-mode.el --- a major-mode for editing Rasid code

(defvar rasid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f5>") 'lua-send-current-line)
    (define-key map (kbd "S-<f5>") 'lua-restart-with-whole-file)
    (define-key map (kbd "<f6>") 'lua-send-region)
    (define-key map (kbd "C-c l") 'lua-send-current-line)
    (define-key map (kbd "C-c C-l") 'lua-send-current-line)
    (define-key map (kbd "C-c c") 'rasid-send-block)
    (define-key map (kbd "C-c C-c") 'rasid-send-block)
    map))

(define-derived-mode rasid-mode lua-mode "Rasid"
  "Major mode for editing Rasid scripts"
  (set (make-local-variable 'lua-default-application) "rasid"))

(defun rasid-send-string (lua-send-string str)
  "Send STR plus an empty comment line (--) to Rasid."
  (funcall lua-send-string (concat str "\n--\n")))

(advice-add 'lua-send-string :around #'rasid-send-string)

(defun rasid-send-block (pos)
  "Send the current block (region delineated by empty lines) to Rasid."
  (interactive "d")
  ; if Rasid is not running, start it and send the whole buffer
  (when (not (process-live-p lua-process))
    (lua-send-buffer))
  (let ((current-line-is-empty (save-excursion
                                 (beginning-of-line)
                                 (looking-at "^[ \t]*$"))))
    (when (not current-line-is-empty)
      (save-excursion
        (let ((start (progn
                       (beginning-of-line)
                       (while (and (> (point) 1)
                                   (not (looking-at "^[ \t]*$")))
                         (forward-line -1))
                       (point)))
              (end (progn
                     (forward-line)
                     (while (and (< (point) (point-max))
                                 (not (looking-at "^[ \t]*$")))
                       (forward-line))
                     (point))))
          (if (and (>= pos start) (< pos end))
              (lua-send-region start end)
            (error "Not inside a block")))))))

(provide 'rasid-mode)
