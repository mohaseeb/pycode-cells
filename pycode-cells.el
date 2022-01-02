;;; package --- Allows splittin python files into cells, each is executable in a python shell


;;; Commentary:

;;; Code:
(defvar pycode-cells-delimiter "#%%")

(defun pycode-cells-go-start ()
  "TODO."
  (evil-ex-search-full-pattern pycode-cells-delimiter 1 'backward)
  ;; TODO return buffer beginning if no match
  (point))

(defun pycode-cells-go-end ()
  "TODO."
  (evil-ex-search-full-pattern pycode-cells-delimiter 1 'forward)
  ;; TODO return buffer end if no match
  (point))

(defun pycode-cells-add-below ()
  "Create a new cell below the current cell."
  (interactive)
  (progn
    (pycode-cells-go-end)
    (+default/newline-below)
    (insert (format "%s" pycode-cells-delimiter))
    (+default/newline-above)))

(defun pycode-cells-add-above ()
  "Create a new cell above the current cell."
  (interactive)
  (progn
    (pycode-cells-go-start)
    (+default/newline-above)
    (insert (format "%s" pycode-cells-delimiter))
    (+default/newline-below)))

(defun pycode-cells-find-cell-start ()
  "Search backword from the cursor position until cell start or buffer start is found."
  (let* ((pos-orig (point))
         (pos-start (pycode-cells-go-start)))
    (progn
      (goto-char pos-orig)
      pos-start)))

(defun pycode-cells-find-cell-end ()
  "Search forward from the cursor position until cell end or buffer end is found."
  (let* ((pos-orig (point))
         (pos-end (pycode-cells-go-end)))
    (progn
      (goto-char pos-orig)
      pos-end)))

(defun pycode-cells-get-cell-text ()
  "Return the text in the current cell."
  (let ((start (pycode-cells-find-cell-start))
        (end (pycode-cells-find-cell-end)))
    (buffer-substring-no-properties start end)))

(defun pycode-cells-send-cell ()
  "Send to the python shel for execution."
  (interactive)
  (python-shell-send-string (pycode-cells-get-cell-text) nil t))

(defun pycode-cells-send-cell-add ()
  "Send to the python shel for execution."
  (interactive)
  (progn
    (pycode-cells-send-cell)            ;; TODO exec below only if this is successful
    (pycode-cells-add-below)))

(provide 'pycode-cells)
;;; pycode-cells.el ends here
