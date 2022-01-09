# pycode-cells
A minimal "jupyter notebook"-like experience on Emacs. 
![pycode-cells-demo](./pycode-cells-demo.gif)

## Installation
**Doom Emacs**

- Clone this repository
- Add below to `package.el`
  ``` emacs-lisp
  (package! python-cells
    :recipe (:local-repo "/path/to/repo/pycode-cells"))
  ```
- Add below to `config.el` 
  ``` emacs-lisp
(use-package pycode-cells)
(map! (:when (featurep! :lang python)
        :leader
        (:prefix ("j" . "pycode-cells")
                :desc "Add cell below" "b" #'pycode-cells-add-below
                :desc "Add cell abvoe" "a" #'pycode-cells-add-above
                :desc "Go cell below" "j" #'pycode-cells-go-below
                :desc "Go cell above" "k" #'pycode-cells-go-above
                :desc "Execute cell" "e" #'pycode-cells-send-cell
                :desc "Execute cell and go below"  "d" #'pycode-cells-send-cell-then-go-below)))
  ```
- Update Doom
  ``` sh
  doom sync
  ```
