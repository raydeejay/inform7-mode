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

;; START

(defun compile-current-story ()
  "Compile the current file (must be saved to disk already)."
(interactive)
    ;; trim '/Source' from the directory to get the parameter to pass to i7
    (save-buffer)
    (setq fName (substring default-directory 0 -8))
    (setq progName "i7")

      (progn
        (message "Compiling...")
	(delete-other-windows)
	(switch-to-buffer-other-window "*Inform7 compile output*")
	(erase-buffer)
        (apply 'make-comint "Inform7 compile output" progName nil (list "-c" fName))
      )

	;; some key bindings for ease of use
        (local-set-key (kbd "q") 'kill-buffer-and-window)
)

(defun run-current-story ()
  "Run the current compiled version of the story."
  (interactive)
  ;;(save-buffer)
  ;; here perhaps we should trigger recompilation?
  
  (setq fName (concat (substring default-directory 0 -8) "/Build/output.z5"))

  (progn
    (message "Running... ('C-q' to exit)")
    ;;(delete-other-windows)
    ;;(setq bufName (concat "Malyon - " fName))
    ;;(switch-to-buffer-other-window bufName)
    ;;(erase-buffer)
    (malyon fName)

    ;; some key bindings for ease of use (local to Malyon buffers :D)
    (local-set-key (kbd "C-q") 'kill-malyon)
    
    ))

(defun kill-malyon ()
  "Close Malyon and split the screen switching buffers, so there's a copy of the transcript visible."
  (interactive)
  (malyon-quit)
  (split-window-right)
  (next-buffer)

)

(defun insert-inform-skeleton ()
  "Creates a basic skeleton for a story (in spanish)"

  (interactive)
  (insert
"Book 1 - The whole stuff

Part 1 - Introducción

The story title is \"Test\".
The story author is \"Sergi Reyner\".
The story headline is \"'Un ejemplo de aventura'\".
The story genre is \"Tutorial\".
The release number is 1.
The story creation year is 2012.
The story description is \"Pequeño mundo de pruebas con Inform 7\".

Section 1 - Extensiones

Include Spanish by Sebastian Arg.
Include Basic Screen Effects Sp by Emily Short.
[Include Basic Help Menu SP by Emily Short.]

Section 2 - Configuración del motor

Use full-length room descriptions.


Part 2 - Localidades

When play begins: say \"Bienvenido a... [line break]\".

The Limbo is a room.

Part 3 - Reparto

Part 4 - Nudo

Part 5 - Vocabulario

" ;; fix emacs higlighting with this --> "
))

(defun insert-inform-room ()
  ""
  (interactive)

  (setq name (read-string "Room name: "))
  (setq answer (read-char-choice "Do you wish to provide a printed name? y/[n]" (append "yn" () )))
  (if (equal answer 121) (setq printed-name (read-string "Printed name: "))
    (setq printed-name nil)
    )
  
  (setq answer (read-char-choice "Is it dark? y/[n]" (append "yn" () )))
  (if (equal answer 110) (setq dark nil)
    (if (equal answer 121) (setq dark 't)
      ))

  (insert (concat "The " name " is a room."
		  (if printed-name (concat " The printed name is \"" printed-name "\"."))
		  (if dark " It is dark.")
		  " The description is \"\"."
		  ))

  ;; position the cursor inside the description string
  (backward-char 2)

)

(defun insert-inform-thing ()
  ""
  (interactive)

  (setq name (read-string "Thing name: "))
  (setq answer (read-char-choice "Do you wish to provide a printed name? y/[n]" (append "yn" () )))
  (if (equal answer 121) (setq printed-name (read-string "Printed name: "))
    (setq printed-name nil)
    )
  
  (setq answer (read-char-choice "Is it somewhere? [y]/n" (append "yn" () )))
  (if (equal answer 110) (setq place nil)
    (if (equal answer 121) (setq place (read-string "Where is it?: "))
      ))

  (setq answer (read-char-choice "Is it fixed in place? y/[n]" (append "yn" () )))
  (if (equal answer 110) (setq fixed nil)
    (if (equal answer 121) (setq fixed 't)
      ))

  (insert (concat "The " name " is a thing" (if place (concat " in " place)) "."
		  (if printed-name (concat " The printed name is \"" printed-name "\"."))
		  (if fixed " It is fixed in place.")
		  " The description is \"\"."
		  ))

  ;; position the cursor inside the description string
  (backward-char 2)

)

(defun insert-inform-container ()
  ""
  (interactive)

  ;; use let so as to not pollute the global namespace
  (let (answer name printed-name place fixed openable open locked)
    (progn
      (setq name (read-string "Container name: "))
      (setq answer (read-char-choice "Do you wish to provide a printed name? y/[n]" (append "yn" () )))
      (if (equal answer 121) (setq printed-name (read-string "Printed name: "))
	(setq printed-name nil)
	)
      
      (setq answer (read-char-choice "Is it somewhere? [y]/n" (append "yn" () )))
      (if (equal answer 110) (setq place nil)
	(if (equal answer 121) (setq place (read-string "Where is it?: "))
	  ))
      
      (setq answer (read-char-choice "Is it fixed in place? y/[n]" (append "yn" () )))
      (if (equal answer 110) (setq fixed nil)
	(if (equal answer 121) (setq fixed 't)
	  ))
      
      (setq answer (read-char-choice "Is it openable? y/[n]" (append "yn" () )))
      (if (equal answer 110) (setq openable nil)
	(if (equal answer 121) (setq openable 't)
	  ))
      
      (setq answer (read-char-choice "Is it open? [y]/n" (append "yn" () )))
      (if (equal answer 110) (setq open nil)
	(if (equal answer 121) (setq open 't)
	  ))
      
      (setq answer (read-char-choice "Is it locked? y/[n]" (append "yn" () )))
      (if (equal answer 110) (setq locked nil)
	(if (equal answer 121) (setq locked 't)
	  ))
      
      (insert (concat "The " name " is a thing" (if place (concat " in " place)) "."
		      (if printed-name (concat " The printed name is \"" printed-name "\"."))
		      (if fixed " It is fixed in place.")
		      (concat " It is " (if (not openable) "un") "openable and " (if open "open" "closed") ".")
		      (if locked " It is locked.")
		      " The description is \"\"."
		      ))
      ))

  ;; position the cursor inside the description string
  (backward-char 2)

)

;; add the hooks when loading the mode
;; this should be moved to the beginning of the file when finished

(add-hook 'inform7-mode-hook
    (lambda ()
      (local-set-key (kbd "C-c C-c") 'compile-current-story)
      (local-set-key (kbd "C-c C-r") 'run-current-story)

      (local-set-key (kbd "C-c S") 'insert-inform-skeleton)

;;      (local-set-key (kbd "C-c I") 'insert-inform-thing)
;;      (local-set-key (kbd "C-c TAB B") 'insert-inform-backdrop)
      (local-set-key (kbd "C-c TAB c") 'insert-inform-container)
;;      (local-set-key (kbd "C-c TAB d") 'insert-inform-door)
;;      (local-set-key (kbd "C-c TAB D") 'insert-inform-direction)
;;      (local-set-key (kbd "C-c TAB p") 'insert-inform-person)
      (local-set-key (kbd "C-c TAB r") 'insert-inform-room)
;;      (local-set-key (kbd "C-c TAB s") 'insert-inform-supporter)
      (local-set-key (kbd "C-c TAB t") 'insert-inform-thing)
;;      (local-set-key (kbd "C-c TAB v") 'insert-inform-device)
;;      (local-set-key (kbd "C-c TAB V") 'insert-inform-vehicle)
;;      (local-set-key (kbd "C-c TAB R") 'insert-inform-region)



))

;; END

(provide 'inform7-mode)

