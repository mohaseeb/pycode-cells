;;; package --- Allows splittin python files into cells, each is executable in a python shell
;;; Code:
(defvar mohaseeb/cell-delimiter "#%%")

(defun mohaseeb/go-cell-start ()
  "TODO."
  (evil-ex-search-full-pattern mohaseeb/cell-delimiter 1 'backward)
  ;; TODO return buffer beginning if no match
  (point))

(defun mohaseeb/go-cell-end ()
  "TODO."
  (evil-ex-search-full-pattern mohaseeb/cell-delimiter 1 'forward)
  ;; TODO return buffer end if no match
  (point))

(defun mohaseeb/find-cell-start ()
  "Search backword from the cursor position until cell start or buffer start is found."
  (let* ((pos-orig (point))
         (pos-start (mohaseeb/go-cell-start)))
    (progn
      (goto-char pos-orig)
      pos-start)))

(defun mohaseeb/find-cell-end ()
  "Search forward from the cursor position until cell end or buffer end is found."
  (let* ((pos-orig (point))
         (pos-end (mohaseeb/go-cell-end)))
    (progn
      (goto-char pos-orig)
      pos-end)))

(defun mohaseeb/get-current-cell-text ()
  "Return the text in the current cell."
  (let ((start (mohaseeb/find-cell-start))
        (end (mohaseeb/find-cell-end)))
    (buffer-substring-no-properties start end)))

(defun mohaseeb/python-shell-send-cell ()
  "Find the current cell text and send to the python shel for execution."
  (interactive)
  (python-shell-send-string (mohaseeb/get-current-cell-text) nil t))

;;; Commentary:
(provide 'mohaseeb/python-shell-send-cell)
;;; emacs-python-cellular.el ends here
