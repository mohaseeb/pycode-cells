;;; package --- Allows splittin python files into cells. Each code cell can be sent to
;;; a python process for execution.

;;; Commentary:

;;; Code:
(defvar pycode-cells-delimiter "#%%")


(defun pycode-cells-go-start ()
  "Go to the cell start; if none, go to the buffer beginning."
  (let* (
         (result (search-forward pycode-cells-delimiter
                        nil
                        t
                        -1))
         (pos (if (equal result nil)
                  (point-min)
                result)))
    (goto-char pos)))


(defun pycode-cells-go-end ()
  "Go to the cell end; if none, go to the buffer end."
  (let* (
         (result (search-forward pycode-cells-delimiter
                        nil
                        t
                        1))
         (pos (if (equal result nil)
                  (point-max)
                result)))
    (goto-char pos)))


(defun pycode-cells-add-below ()
  "Create a new cell below the current cell."
  (interactive)
  (progn
    (pycode-cells-go-end) ;; FIXME add delimiter to last cell if missing
    (+default/newline-below)
    (insert (format "%s" pycode-cells-delimiter))
    (+default/newline-above)))

(defun pycode-cells-add-above ()
  "Create a new cell above the current cell."
  (interactive)
  (progn
    (pycode-cells-go-start) ;; FIXME add delimiter to first cell if missing
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

(defun pycode-cells-go-above ()
  "Move the cursor to the first line of the cell above."
  (interactive)
  (progn
    (pycode-cells-go-start)
    (pycode-cells-go-start)
    (forward-line)))

(defun pycode-cells-go-below ()
  "Move the cursor to the first line of the cell below."
  (interactive)
  (progn
    (pycode-cells-go-end)
    (forward-line)))

(provide 'pycode-cells)
;;; pycode-cells.el ends here
