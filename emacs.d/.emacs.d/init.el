 ;;; -*- lexical-binding: t -*-

;; Get packages
(require 'use-package)
(setq use-package-always-ensure t)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("GNU ELPA"     . 15)
        ("MELPA"        . 10)
        ("MELPA Stable" . 5)
        ("nongnu"       . 0)))

;; Supress warnings
(setq native-comp-async-report-warnings-errors 'silent) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Stop garbage collection when opening emacs
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20))))

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode'
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; Nerd fonts
(use-package nerd-icons)

;; Dolist 0
(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text
           scroll-bar-mode              ; No scroll bars either
           blink-cursor-mode
	       menu-bar-mode))              ; The blinking cursor gets old
  (funcall mode 0))

;; Dolist 1
(dolist (mode
         '(global-display-line-numbers-mode ; Line numbers
           column-number-mode           ; Column numbers
           pixel-scroll-precision-mode  ; Smooth scrolling
           electric-pair-mode           ; Pair parentheseses etc
           global-visual-line-mode      ; Wrap long lines
           ))
  (funcall mode 1))

;; Yes/no => y/n
(setopt use-short-answers t)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; UTF-8
(set-language-environment    "UTF-8")
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

;; Writing
(setq-default tab-width 4                       ; Smaller tabs
              fill-column 79                    ; Maximum line width
              truncate-lines t                  ; Don't fold lines
              indent-tabs-mode nil              ; Use spaces instead of tabs
              split-width-threshold 160         ; Split verticly by default
              split-height-threshold nil        ; Split verticly by default
              frame-resize-pixelwise t          ; Fine-grained frame resize
              auto-fill-function 'do-auto-fill) ; Auto-fill-mode everywhere


;; Center text
(use-package olivetti
  :defer t
  :bind ("C-c o" . olivetti-mode)
  :config
  (setq-default olivetti-body-width (+ fill-column 3)))

;; Prevent plugins from messing up my config
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Better undoing
(use-package undo-fu
  :defer t
  )
;; Themes
(defun disable-custom-themes (theme &optional no-confirm no-enable)
  (mapc 'disable-theme custom-enabled-themes))

(advice-add 'load-theme :before #'disable-custom-themes)

(setq custom-theme-directory "~/.emacs.d/custom-theme")
(setq custom-safe-theme t)

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin :no-confirm)
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload)
  )

;; Better modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-nil)
  )

;; Dashboard
(use-package dashboard
  :config
  (setq dashboard-projects-backend 'project-el
        dashboard-startup-banner "~/.emacs.d/images/vim.png"
        dashboard-banner-logo-title nil
        dashboard-center-content t
        dashboard-page-separator "\n\n\n"
        dashboard-items '((recents   . 20)
                          (bookmarks . 20)))
  (dashboard-setup-startup-hook))

;; Fonts and ligatures
(use-package nerd-icons)

(when (member "Fira Code" (font-family-list))
  (set-face-attribute 'default nil :font "Fira Code" :height 115))

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                               ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                               "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                               "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                               "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                               "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                               "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                               "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                               "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                               "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~"
                               "~~>" "%%"))
  (global-ligature-mode 't)
  )

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  )

;; Autosave
(defvar emacs-autosave-directory
  "~/.emacs-autosave/"
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

;; Command overview and searching
(defun take-me-home ()
  (interactive)
  (if (looking-back "/" nil)
      (progn (call-interactively 'delete-minibuffer-contents) (insert "~/"))
    (call-interactively 'self-insert-command)))
(use-package vertico
  :bind (:map vertico-map ("~" . take-me-home))
  :config
  (vertico-mode)
  (setq vertico-count 25))
(use-package vertico-posframe
  :init
  (setq vertico-posframe-parameters   '((left-fringe  . 12)    ;; Fringes
                                        (right-fringe . 12)
                                        (undecorated  . nil)
                                        (border-width . 3))) ;; Rounded frame
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-width        88                       ;; Narrow frame
        vertico-posframe-height       vertico-count            ;; Default height
        ;; Don't create posframe for these commands
        vertico-multiform-commands    '((consult-line    (:not posframe))
                                        (consult-ripgrep (:not posframe)))))
(use-package orderless
  :config
  (setq completion-styles '(orderless basic partial-completion)
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator "[ |]"))
(use-package marginalia
  :init
  (marginalia-mode 1))
(use-package which-key
  :config
  (which-key-mode))
(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-c r"   . consult-ripgrep))
  :config
  (setq consult-preview-key (list :debounce 0.1 'any)))

;; Git stuff
(use-package magit
  :hook ((magit-pre-refresh . ignore)    ;; diff-hl-magit-pre-refresh is obsolete
         (magit-post-refresh . ignore))  ;; diff-hl-magit-post-refresh is obsolete
  :bind ("C-c m" . magit-status))
(use-package diff-hl
  :config
  (global-diff-hl-mode 1))
(use-package blamer
  :after magit
  :bind (
         ("C-c g i" . blamer-show-commit-info)
         ("C-c g b" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time                 0.3)
  (blamer-min-offset                4)
  (blamer-max-commit-message-length 100)
  (blamer-datetime-formatter        "[%s]")
  (blamer-commit-formatter          " ● %s")
  (global-blamer-mode               1)
  :custom-face
  (blamer-face ((t :foreground "#008b8b"
                   :background nil
                   :height 115
                   :italic nil))))

;; Move lines
(use-package move-text
  :bind (
         ("C-M-<down>" . move-text-down)
         ("C-M-<up>" . move-text-up)))

;; Spell check
(use-package jinx
  :hook (org-mode . jinx-mode)
  :bind ("C-." . jinx-correct)
  :config
  (setq jinx-languages "en_US nb-NO")
  )

;; Commenting
(use-package evil-nerd-commenter
  :defer t
  :bind ("C-;" . evilnc-comment-or-uncomment-lines))

;;; LSP/autocomplete
(use-package eglot
  :defer t
  :hook ((python-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (markdown-mode . eglot-ensure)
         )
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver"))
               '(java-mode . ("jdtls"))))
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . (lambda () (add-hook 'after-save-hook
                                          #'eglot-format-buffer
                                          nil t)))
  )
(use-package corfu
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  :config
  (setopt text-mode-ispell-word-completion nil) ; TODO make this work
  )
(use-package cape
  :init
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-keyword))

;; Note taking
(use-package pdf-tools ; Show PDFs
  :defer t
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("c" . (lambda ()
                       (interactive)
                       (if header-line-format
                           (setq header-line-format nil)
                         (nano-modeline-pdf-mode))))
              ("j" . pdf-view-next-page)
              ("k" . pdf-view-previous-page))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))
(defun disable-line-numbers-for-pdf ()
  "Disable line numbers when opening PDFs."
  (when (string= (file-name-extension (buffer-file-name)) "pdf")
    (display-line-numbers-mode -1)))

(add-hook 'pdf-view-mode-hook 'disable-line-numbers-for-pdf)

(defun my-markdown-to-pdf ()
  "Save current buffer and export markdown to PDF using Pandoc."
  (interactive)
  (let* ((filename (buffer-file-name))
         (basename (file-name-base filename))
         (output-dir "pdf/")
         (output-file (concat output-dir basename ".pdf")))
    (unless (file-directory-p output-dir)
      (make-directory output-dir))
    (save-buffer)
    (start-process "pandoc-process" "*pandoc-output*"
                   "pandoc" filename "-o" output-file)))

;; Bind the function to a key combo, e.g., C-c p
(global-set-key (kbd "C-c p") 'my-markdown-to-pdf)

(defun auto-revert-pdf-on-change () ; Auto-revert PDF files
  "Enable auto-reverting for PDF files."
  (when (and (buffer-file-name)
             (string= (file-name-extension (buffer-file-name)) "pdf"))
    (auto-revert-mode 1)))

(add-hook 'find-file-hook 'auto-revert-pdf-on-change)

;; Better ctrl-backspace (kill word)
(defun ryanmarcus/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-back "[ \n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[ \n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (backward-kill-word 1)))
(global-set-key (kbd "C-<backspace>") 'ryanmarcus/backward-kill-word)

;; Evil mode
(use-package evil
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-fu)
  )
(setopt display-line-numbers-type 'relative)

;; Org mode
(use-package org
  :defer t
  :hook (org-mode . olivetti-mode)
  :bind (:map org-mode-map
              ("C-c C-c" . org-latex-export-to-pdf))
  :config
  (setq org-adapt-indentation t
        org-hide-leading-stars t
        org-pretty-entities t
        org-startup-folded 'showeverything
        org-src-fontify-natively t
	    org-src-tab-acts-natively t
        org-edit-src-content-indentation 0)
  
  ;; Resize Org headings
  (custom-set-faces
  '(org-document-title ((t (:height 1.6))))
  '(outline-1          ((t (:height 1.25))))
  '(outline-2          ((t (:height 1.2))))
  '(outline-3          ((t (:height 1.15))))
  '(outline-4          ((t (:height 1.1))))
  '(outline-5          ((t (:height 1.1))))
  '(outline-6          ((t (:height 1.1))))
  '(outline-8          ((t (:height 1.1))))
  '(outline-9          ((t (:height 1.1))))))

;; Markdown
(use-package markdown-mode)

;; Editorconfig
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1)
  )
(setq-default tab-width 4)
