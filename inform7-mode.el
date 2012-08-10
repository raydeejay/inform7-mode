(require 'sws-mode)

(defface inform7-heading-face
  '((t (:inherit font-lock-preprocessor-face :weight bold :height 1.2)))
  "Face for Inform 7 headings"
  :group 'font-lock-highlighting-faces)

(defvar inform7-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'newline-and-indent)
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for inform7 major mode")

(defadvice sws-do-indent-line (around inform7-indent-blank () activate)
  "Ensure proper indentation of blank lines according to Inform7 conventions."
  (if (and (eq major-mode 'inform7-mode) (sws-empty-line-p))
    (if (save-excursion
          (previous-line)
          (looking-at "^.*:[ \t]*$"))
        (indent-to (sws-max-indent))
      (indent-to (sws-previous-indentation)))
    ad-do-it))

(defconst inform7-font-lock-keywords
  `(( ,(regexp-opt '("let" "say" "if" "otherwise") 'words) . font-lock-keyword-face)
    ("^\\(\\(?:Book\\|Chapter\\|Part\\|Section\\|Volume\\) - .*\\)" . 'inform7-heading-face)
    (".\\(\\[.*?\\]\\)." 0 font-lock-variable-name-face t)
    )
  "Highlighting expressions for inform7-mode")

(define-derived-mode inform7-mode sws-mode "Inform7"
  "Major mode for editing inform 7 story files."
  (visual-line-mode)
  (set (make-local-variable 'font-lock-defaults) '(inform7-font-lock-keywords nil t)))

(modify-syntax-entry ?\[ "<]" inform7-mode-syntax-table)
(modify-syntax-entry ?\] ">[" inform7-mode-syntax-table)


;; raydj
(require 'comint)

(defun compile-current-story ()
  "Compile the current file (must be saved to disk already)."
(interactive)
    ; trim '/Source' from the directory to get the parameter to pass to i7
    (save-buffer)
    (setq fName (substring default-directory 0 -8))
    (setq progName "i7")
    (setq cmdStr (concat progName " \""   fName "\""))

      (progn
        (message "Compiling...")
;        (shell-command cmdStr "*Inform7 compile output*" )
	(delete-other-windows)
	(switch-to-buffer-other-window "*Inform7 compile output*")
	(erase-buffer)
	(other-window -1)
        (apply 'make-comint "Inform7 compile output" progName nil (list "-c" fName))
        )
)

(global-set-key (kbd "C-c C-c") 'compile-current-story)

; (delete-other-windows-internal)

(provide 'inform7-mode)
