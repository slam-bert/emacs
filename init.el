(set-face-attribute 'default nil :font "Cascadia Code-14")

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; Straight Package Manager (replaces package.el)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package tree-sitter
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-banner-logo-title "Editor MACroS!"
        dashboard-startup-banner 'official
        dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :init
  (setq size-indication-mode t
        doom-modeline-height 40
        doom-modeline-hud nil
        doom-modeline-buffer-file-name-style 'truncate-nil
        doom-modeline-major-mode-icon nil
        doom-modeline-unicode-fallback t
        doom-modeline-enable-word-count t
	doom-modeline-bar-width 8
        doom-modeline-indent-info t)
  :config
  (doom-modeline-mode 1))

(use-package autothemer)
(use-package catppuccin-theme
  :init
  (load-theme 'catppuccin-frappe t))

(use-package smart-cursor-color
  :init
  (smart-cursor-color-mode +1))

(use-package haskell-mode)
(use-package lua-mode)
(use-package markdown-mode)