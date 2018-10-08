;;; rasid-mode.el --- a major-mode for editing Rasid code

(defvar rasid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f2>") 'rasid-send-block-and-play-next-item)
    (define-key map (kbd "S-<f2>") 'rasid-send-block-and-play-next-item-forever)
    (define-key map (kbd "M-<f2>") 'rasid-stop)
    (define-key map (kbd "<f3>") 'rasid-send-line)
    (define-key map (kbd "<f4>") 'rasid-send-block)
    (define-key map (kbd "C-c c") 'rasid-send-block)
    (define-key map (kbd "C-c C-c") 'rasid-send-block)
    (define-key map (kbd "<f5>") 'rasid-restart)
    (define-key map (kbd "<f6>") 'rasid-send-region)
    map))

(define-derived-mode rasid-mode lua-mode "Rasid"
  "Major mode for editing Rasid scripts")

(defun rasid-start-process ()
  (interactive)
  (setq lua-process-buffer (make-comint "rasid" "rasid"))
  (setq lua-process (get-buffer-process lua-process-buffer))
  (set-process-query-on-exit-flag lua-process nil)
  (if (called-interactively-p 'any)
      (switch-to-buffer lua-process-buffer)))

(defun rasid-get-create-process ()
  "Return active Lua process creating one if necessary."
  (or (and (comint-check-proc lua-process-buffer)
           lua-process)
      (rasid-start-process))
  lua-process)

(defun rasid-send-string (str)
  (process-send-string (rasid-get-create-process)
                       (concat str "\n--\n")))

(defun rasid-send-region (start end)
  (interactive "r")
  (rasid-send-string (buffer-substring-no-properties start end))
  (when lua-always-show (lua-show-process-buffer)))

(defun rasid-send-buffer ()
  "Send whole buffer to lua subprocess."
  (interactive)
  (rasid-send-region (point-min) (point-max)))

(defun rasid-send-line ()
  "Send the current line to Rasid."
  (interactive)
  (when (not (process-live-p lua-process))
    (rasid-send-buffer))
  (rasid-send-region (line-beginning-position) (line-end-position)))

(defun rasid-send-block (pos)
  "Send the current block (region delineated by empty lines) to Rasid."
  (interactive "d")
  ; if Rasid is not running, start it and send the whole buffer
  (when (not (process-live-p lua-process))
    (rasid-send-buffer))
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
              (rasid-send-region start end)
            (error "Not inside a block")))))))

(defun rasid-send-block-and-play-next-item (pos)
  (interactive "d")
  (rasid-send-block pos)
  (rasid-send-string "R.play()"))

(defun rasid-send-block-and-play-next-item-forever (pos)
  (interactive "d")
  (rasid-send-block pos)
  (rasid-send-string "R.play(nil, true)"))

(defun rasid-stop ()
  (interactive)
  (rasid-send-string "R.stop()"))

(defun rasid-restart ()
  "Restart lua subprocess and send whole file as input."
  (interactive)
  (lua-kill-process)
  (rasid-send-buffer))

(provide 'rasid-mode)
