;;; package --- Allows splittin python files into cells. Each code cell can be sent to
;;; a python process for execution.

;;; Commentary:

;;; Code:
(defvar pycode-cells-delimiter "#%%")

(defun pycode-cells-go-start ()
  "Go to the cell start; if none, go to the buffer beginning."
  (let* (
         (result (search-forward pycode-cells-delimiter nil t -1))
         (pos (if (equal result nil) (point-min) result)))
    (goto-char pos)))

(defun pycode-cells-go-end ()
  "Go to the cell end; if none, go to the buffer end."
  (let* (
         (result (search-forward pycode-cells-delimiter nil t 1))
         (pos (if (equal result nil) (point-max) result)))
    (goto-char pos)))

(defun pycode-cells-require-delimiter (add-above?)
  "Add cell delimiter below/above current line if missing depending on the value of ADD-ABOVE?."
  (if (not (pycode-cells-line-has-delimiter?))
      (progn
        (if add-above? (+default/newline-above) (+default/newline-below))
        (insert (format "%s" pycode-cells-delimiter)))))

(defun pycode-cells-line-has-delimiter? ()
  "Return the pos of delilimiter in the current line, nil if the delimiter is missing."
  (string-match-p pycode-cells-delimiter (thing-at-point 'line)))

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

(defun pycode-cells-add-below ()
  "Create a new cell below the current cell."
  (interactive)
  (progn
    (pycode-cells-go-end)
    (pycode-cells-require-delimiter nil)
    (+default/newline-below)
    (insert (format "%s" pycode-cells-delimiter))
    (+default/newline-above)))

(defun pycode-cells-add-above ()
  "Create a new cell above the current cell."
  (interactive)
  (progn
    (pycode-cells-go-start)
    (pycode-cells-require-delimiter t)
    (+default/newline-above)
    (insert (format "%s" pycode-cells-delimiter))
    (+default/newline-below)))

(defun pycode-cells-send-cell ()
  "Send to the python process for execution."
  (interactive)
  (python-shell-send-string (pycode-cells-get-cell-text) nil t))

(defun pycode-cells-send-cell-then-go-below ()
  "Send to the python process for execution then add a new cell below."
  (interactive)
  (progn
    (pycode-cells-send-cell)            ;; TODO exec below only if this is successful
    (pycode-cells-go-below)
    (pycode-cells-make-cell-if-not)))

(defun pycode-cells-make-cell-if-not()
  "Ensure the region from current poisition until the end of buffer consitites a cell."
  (save-excursion
   (progn
     (if (equal (point) (point-max)) (save-excursion (+default/newline-below)))
     (if (equal nil (search-forward pycode-cells-delimiter nil t 1))
         (progn
           (goto-char (- (point-max) 1))
           (pycode-cells-require-delimiter nil))))))


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
