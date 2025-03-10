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

;; Dolist 0
(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text
           scroll-bar-mode              ; No scroll bars either
           blink-cursor-mode
	       menu-bar-mode))          ; The blinking cursor gets old
  (funcall mode 0))

;; Dolist 1
(dolist (mode
         '(display-line-numbers-mode
           ))
  (funcall mode 1))

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

;; Prevent plugins from messing up my config
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Better undoing
(use-package undo-fu
  :defer t
  :bind (
              ("C-z" . undo-fu-only-undo)
              ("M-z" . undo-fu-only-redo)))

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

;; Dashboard
(use-package dashboard
  :config
  (setq dashboard-projects-backend 'project-el
        dashboard-startup-banner "~/.emacs.d/images/vim.png"
        dashboard-banner-logo-title nil
        dashboard-center-content t
        dashboard-page-separator "\n\n\n"
        dashboard-items '((projects  . 15)
                          (recents   . 10)
                          (bookmarks . 5)))
  (dashboard-setup-startup-hook))

;; Font and ligatures
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

;; Command overview
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
(use-package marginalia
  :init 
  (marginalia-mode 1))
(use-package which-key
  :config
  (which-key-mode))

;; Code autocomplete
(use-package corfu
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-cycle t))

;; Git stuff
(use-package magit
  :hook ((magit-pre-refresh . ignore)    ;; diff-hl-magit-pre-refresh is obsolete
         (magit-post-refresh . ignore))  ;; diff-hl-magit-post-refresh is obsolete
  :bind ("C-c m" . magit-status))
(use-package diff-hl
  :config
  (global-diff-hl-mode 1 i
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
  :custom-face
  (blamer-face ((t :foreground "#008b8b"
                    :background nil
                    :height 1
                    :italic nil))))

