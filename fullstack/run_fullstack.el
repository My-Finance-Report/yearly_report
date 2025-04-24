(defun start-vterm (name command)
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (vterm-mode)
      (vterm-send-string command t)
      (vterm-send-return))
    (switch-to-buffer buf)))

(start-vterm "backend" "bin/backend")
(start-vterm "frontend" "bin/frontend")
(start-vterm "worker" "bin/worker")

